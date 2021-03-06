/*
 * Functions for transferring packet data over an Intel FPGA Avalon FIFO
 * Memory Core.
 *
 * (C) Copyright 2019,2020 QBayLogic B.V.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * ALTERNATIVELY, this product may be distributed under the terms of the GNU
 * General Public License, either version 2 of the License, or (at your
 * option) any later version, in which case the provisions of the GPL are
 * required INSTEAD OF the above restrictions.  (This clause is necessary due
 * to a potential bad interaction between the GPL and the restrictions
 * contained in a BSD-style copyright.)
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
#include "packet_fifo.h"

#include <arpa/inet.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#include "avalon_fifo.h"
#include "mmio.h"
#include "uio_helper.h"

static int
match_uio_attr(const struct uio_info_t *info, const char *name,
		const char *value)
{
	const struct uio_dev_attr_t *attr;
	for (attr = info->dev_attrs; attr; attr = attr->next) {
		if (!strcmp(attr->name, name) && !strcmp(attr->value, value))
			return 1;
	}
	return 0;
}

/*
 * In uevent files, the kernel might number an attribute that can have
 * multiple values. This function checks any of the numbered entries for
 * equality. With `name` = 'uevent/OF_ALIAS_', it will check if there is an
 * entry that has value `value` under all entries with names
 * 'uevent/OF_ALIAS_0', 'uevent/OF_ALIAS_1', etcetera, for all decimal values
 * 0, 1, etcetera.
 */
static int
match_uio_attr_n(const struct uio_info_t *info, const char *name,
		const char *value)
{
	const struct uio_dev_attr_t *attr;
	const char *s;
	size_t prefix_len;

	prefix_len = strlen(name);
	for (attr = info->dev_attrs; attr; attr = attr->next) {
		if (!strncmp(attr->name, name, prefix_len)) {
			s = &attr->name[prefix_len];
			if (!*s)
				continue;
			while (*s >= '0' && *s <= '9')
				s++;
			if (*s)
				continue;
			if (!strcmp(attr->value, value))
				return 1;
		}
	}
	return 0;
}

static char *
get_of_fullname(const struct uio_info_t *info)
{
	const struct uio_dev_attr_t *attr;
	for (attr = info->dev_attrs; attr; attr = attr->next)
		if (!strcmp(attr->name, "uevent/OF_FULLNAME"))
			return (char *) attr->value;
	return NULL;
}

struct uio_info_t *
fifo_uio_by_of_name(const struct uio_info_t *info, const char *of_name)
{
	/* TODO: We should check the uio device version number as well */
	const char prefix[] = "altera_fifo_";
	while (info) {
		if (strncmp(info->name, prefix, strlen(prefix))) {
			info = info->next;
			continue;
		}
		if (of_name[0] == '/') {
			if (match_uio_attr(info, "uevent/OF_FULLNAME",
						of_name))
				return (struct uio_info_t *) info;
		} else {
			if (match_uio_attr_n(info, "uevent/OF_ALIAS_",
						of_name))
				return (struct uio_info_t *) info;
		}
		info = info->next;
	}
	return NULL;
}

int
init_rdfifo(struct rdfifo_ctx **ctx, const struct uio_info_t *info,
		size_t maxpkt)
{
	char uio_dev_n[16];
	const char *csr_name;
	int ret;
	size_t bufsize, i;

	bufsize = (maxpkt + 3) >> 2;
	*ctx = malloc(sizeof(**ctx) + sizeof(uint32_t[bufsize]));
	if (!*ctx) {
		ret = FIFO_GENERAL_ERROR;
		goto init_rd_out_err;
	}
	(*ctx)->next = 0;
	(*ctx)->bufsize = bufsize;
	(*ctx)->word_cnt = 0;

	ret=FIFO_DEV_ERROR;
	if (!strcmp(info->name, "altera_fifo_in_irq"))
		csr_name = "in_csr";
	else if (!strcmp(info->name, "altera_fifo_out_irq"))
		csr_name = "out_csr";
	else
		goto init_rd_out_free_ctx;

	sprintf(uio_dev_n, "/dev/uio%d", info->uio_num);
	if (((*ctx)->uio_fd=open(uio_dev_n, O_RDWR)) == -1)
		goto init_rd_out_free_ctx;

	for (i = 0; i < MAX_UIO_MAPS; i++)
		if (!strcmp(info->maps[i].name, "out"))
			break;
	if (i == MAX_UIO_MAPS)
		goto init_rd_out_close;
	(*ctx)->out.map_base = mmap(NULL, info->maps[i].size,
			(PROT_READ | PROT_WRITE), MAP_SHARED, (*ctx)->uio_fd,
			i * getpagesize());
	if ((*ctx)->out.map_base == MAP_FAILED)
		goto init_rd_out_close;
	(*ctx)->out.size = info->maps[i].size;
	(*ctx)->out.reg_base = addr_offset((*ctx)->out.map_base,
			info->maps[i].offset);

	for (i = 0; i < MAX_UIO_MAPS; i++)
		if (!strcmp(info->maps[i].name, csr_name))
			break;
	if (i == MAX_UIO_MAPS)
		goto init_rd_out_mmap1;
	(*ctx)->csr.map_base = mmap(NULL, info->maps[i].size,
			(PROT_READ | PROT_WRITE), MAP_SHARED, (*ctx)->uio_fd,
			i * getpagesize());
	if ((*ctx)->csr.map_base == MAP_FAILED)
		goto init_rd_out_mmap1;
	(*ctx)->csr.size = info->maps[i].size;
	(*ctx)->csr.reg_base = addr_offset((*ctx)->csr.map_base,
			info->maps[i].offset);

	mmio_write32((*ctx)->csr.reg_base, FIFO_IENABLE_REG, 0);
	mmio_write32((*ctx)->csr.reg_base, FIFO_ALMOSTFULL_REG, 1);
	mmio_write32((*ctx)->csr.reg_base, FIFO_EVENT_REG, FIFO_EVENT_ALL);

	return 0;
init_rd_out_mmap1:
	munmap((*ctx)->out.map_base, (*ctx)->out.size);
init_rd_out_close:
	close((*ctx)->uio_fd);
init_rd_out_free_ctx:
	free(*ctx);
init_rd_out_err:
	return ret;
}

void
close_rdfifo(struct rdfifo_ctx *ctx)
{
	munmap(ctx->csr.map_base, ctx->csr.size);
	munmap(ctx->out.map_base, ctx->out.size);
	close(ctx->uio_fd);
	free(ctx);
}

int
init_wrfifo(struct wrfifo_ctx **ctx, const struct uio_info_t *info)
{
	char uio_dev_n[16];
	int ret;
	size_t i;

	*ctx = malloc(sizeof(**ctx));
	if (!*ctx) {
		ret = FIFO_GENERAL_ERROR;
		goto init_wr_out_err;
	}

	ret=FIFO_DEV_ERROR;
	sprintf(uio_dev_n, "/dev/uio%d", info->uio_num);
	if (((*ctx)->uio_fd=open(uio_dev_n, O_RDWR)) == -1)
		goto init_wr_out_free_ctx;

	for (i = 0; i < MAX_UIO_MAPS; i++)
		if (!strcmp(info->maps[i].name, "in"))
			break;
	if (i == MAX_UIO_MAPS) {
		const char *fullname;
		if (info->maps[1].size > 0) {
			/* More than one mapping and none called "in" */
			goto init_wr_out_close;
		}
		fullname = get_of_fullname(info);
		if (!fullname || *fullname == '\0')
			goto init_wr_out_close;
		if (strcmp(info->maps[0].name, fullname))
			goto init_wr_out_close;
		/* Just one mapping and it is anonymous: no reg-name property
		 * means that the name is set to the device's full device-tree
		 * path. Assume it is our "in" register.
		 */
		i = 0;
	}
	(*ctx)->in.map_base = mmap(NULL, info->maps[i].size,
			(PROT_READ | PROT_WRITE), MAP_SHARED, (*ctx)->uio_fd,
			i * getpagesize());
	if ((*ctx)->in.map_base == MAP_FAILED)
		goto init_wr_out_close;
	(*ctx)->in.size = info->maps[i].size;
	(*ctx)->in.reg_base = addr_offset((*ctx)->in.map_base,
			info->maps[i].offset);

	for (i = 0; i < MAX_UIO_MAPS; i++)
		if (!strcmp(info->maps[i].name, "in_csr"))
			break;
	if (i != MAX_UIO_MAPS) {
		(*ctx)->csr.map_base = mmap(NULL, info->maps[i].size,
				(PROT_READ | PROT_WRITE), MAP_SHARED,
				(*ctx)->uio_fd, i * getpagesize());
		if ((*ctx)->csr.map_base == MAP_FAILED)
			goto init_wr_out_mmap1;
		(*ctx)->csr.size = info->maps[i].size;
		(*ctx)->csr.reg_base = addr_offset((*ctx)->csr.map_base,
				info->maps[i].offset);
		/* INT32_MAX is unambiguously positive and large enough. */
		mmio_write32((*ctx)->csr.reg_base, FIFO_ALMOSTEMPTY_REG,
				INT32_MAX);
		(*ctx)->depth = ((size_t) mmio_read32((*ctx)->csr.reg_base,
					FIFO_ALMOSTEMPTY_REG)) + 1;
		mmio_write32((*ctx)->csr.reg_base, FIFO_IENABLE_REG, 0);
		mmio_write32((*ctx)->csr.reg_base, FIFO_ALMOSTEMPTY_REG,
				(*ctx)->depth / 2);
		mmio_write32((*ctx)->csr.reg_base, FIFO_EVENT_REG,
				FIFO_EVENT_ALL);

	} else {
		(*ctx)->csr.reg_base = NULL;
		(*ctx)->depth = -1;
	}

	if (!strcmp(info->name, "altera_fifo_in_irq") &&
			(*ctx)->csr.reg_base) {
		(*ctx)->mode = FIFO_INTR;
	} else if (!strcmp(info->name, "altera_fifo_no_irq")) {
		(*ctx)->mode = FIFO_FREE_RUNNING;
		close((*ctx)->uio_fd);
		(*ctx)->uio_fd = -1;
	} else {
		goto init_wr_out_mmap2;
	}

	return 0;
init_wr_out_mmap2:
	if ((*ctx)->csr.reg_base)
		munmap((*ctx)->csr.map_base, (*ctx)->csr.size);
init_wr_out_mmap1:
	munmap((*ctx)->in.map_base, (*ctx)->in.size);
init_wr_out_close:
	close((*ctx)->uio_fd);
init_wr_out_free_ctx:
	free(*ctx);
init_wr_out_err:
	return ret;
}

void
close_wrfifo(struct wrfifo_ctx *ctx)
{
	munmap(ctx->in.map_base, ctx->in.size);
	if (ctx->csr.reg_base)
		munmap(ctx->csr.map_base, ctx->csr.size);
	if (ctx->uio_fd != -1)
		close(ctx->uio_fd);
	free(ctx);
}

ssize_t
set_wrfifo_thresh(const struct wrfifo_ctx *ctx, size_t thresh)
{
	if (!ctx->csr.reg_base)
		return FIFO_GENERAL_ERROR;
	if (thresh > INT32_MAX)
		/* INT32_MAX is unambiguously positive and large enough. */
		thresh = INT32_MAX;
	mmio_write32(ctx->csr.reg_base, FIFO_ALMOSTEMPTY_REG,
			(uint32_t) thresh);
	return (ssize_t) mmio_read32((ctx)->csr.reg_base,
			FIFO_ALMOSTEMPTY_REG);
}

/*
 * Flush until start of packet
 */
static void
flush_to_sop(struct rdfifo_ctx *ctx, uint32_t *data, uint32_t *other,
		uint32_t *num_elems)
{
	while (!(*other & FIFO_INFO_SOP) && (*num_elems != 0)) {
		*data = mmio_read32(ctx->out.reg_base, FIFO_DATA_REG);
		*other = mmio_read32(ctx->out.reg_base,
				FIFO_OTHER_INFO_REG);
		ctx->word_cnt++;
		if (--*num_elems == 0) {
			mmio_write32(ctx->csr.reg_base, FIFO_EVENT_REG,
					FIFO_EVENT_AF);
			*num_elems = mmio_read32(ctx->csr.reg_base,
					FIFO_LEVEL_REG);
		}
	}
}

int
fifo_read(struct rdfifo_ctx *ctx)
{
	uint32_t data, num_elems;
	uint32_t other = 0;

	mmio_write32(ctx->csr.reg_base, FIFO_EVENT_REG, FIFO_EVENT_AF);
	num_elems = mmio_read32(ctx->csr.reg_base, FIFO_LEVEL_REG);
	if (num_elems == 0) {
		mmio_write32(ctx->csr.reg_base, FIFO_IENABLE_REG,
				FIFO_IENABLE_AF);
		return FIFO_NEED_MORE;
	}
	data = mmio_read32(ctx->out.reg_base, FIFO_DATA_REG);
	other = mmio_read32(ctx->out.reg_base, FIFO_OTHER_INFO_REG);
	ctx->word_cnt++;
	num_elems--;

	if (ctx->next == 0) {
		flush_to_sop(ctx, &data, &other, &num_elems);
		if (!(other & FIFO_INFO_SOP)) {
			mmio_write32(ctx->csr.reg_base, FIFO_IENABLE_REG,
					FIFO_IENABLE_AF);
			return FIFO_NEED_MORE;
		}
	}

	ctx->buf[ctx->next++] = htonl(data);
	while (!(other & FIFO_INFO_EOP) && (num_elems != 0)) {
		if (ctx->next == ctx->bufsize) {
			ctx->next = 0;
			return FIFO_OVERLONG_ERROR;
		}
		ctx->buf[ctx->next++] = htonl(mmio_read32(ctx->out.reg_base,
				FIFO_DATA_REG));
		other = mmio_read32(ctx->out.reg_base, FIFO_OTHER_INFO_REG);
		ctx->word_cnt++;
		if (--num_elems == 0) {
			mmio_write32(ctx->csr.reg_base, FIFO_EVENT_REG,
					FIFO_EVENT_AF);
			num_elems = mmio_read32(ctx->csr.reg_base,
					FIFO_LEVEL_REG);
		}
	}

	if (!(other & FIFO_INFO_EOP)) {
		mmio_write32(ctx->csr.reg_base, FIFO_IENABLE_REG,
				FIFO_IENABLE_AF);
		return FIFO_NEED_MORE;
	}

	ctx->numbytes = ctx->next * 4 - FIFO_INFO_EMPTY_GET(other);
	ctx->next = 0;
	return 0;
}

/*
 * Write 32-bit words to the FIFO.
 *
 * @param ctx The write context for the FIFO.
 * @param buf The data to be written.
 * @param len The number of 32-bit words(!) to write.
 * @returns The number of 32-bit words(!) written.
 */
static size_t
write_words(const struct wrfifo_ctx *ctx, const uint8_t *buf8,
		size_t len, size_t *free_words)
{
	size_t i;
	uint32_t word;

	for (i = 0; i < len;) {
		memcpy(&word, &buf8[i*4], 4);
		mmio_write32(ctx->in.reg_base, FIFO_DATA_REG,
				ntohl(word));
		i++;
		if (!--(*free_words)) {
			mmio_write32(ctx->csr.reg_base, FIFO_EVENT_REG,
					FIFO_EVENT_AE);
			*free_words = ctx->depth - (size_t)
					mmio_read32(ctx->csr.reg_base,
					FIFO_LEVEL_REG);
			if (!*free_words)
				break;
		}
	}
	return i;
}


size_t
fifo_write(const struct wrfifo_ctx *ctx, const void *buf, size_t len)
{
	const uint8_t *buf8 = (const uint8_t *) buf;
	uint32_t word;
	size_t num_words, num_tail, free_words, i;

	if (ctx->mode == FIFO_INTR) {
		mmio_write32(ctx->csr.reg_base, FIFO_EVENT_REG,
				FIFO_EVENT_AE);
		free_words = ctx->depth - (size_t)
				mmio_read32(ctx->csr.reg_base,
				FIFO_LEVEL_REG);
		if (!free_words) {
			mmio_write32(ctx->csr.reg_base, FIFO_IENABLE_REG,
					FIFO_IENABLE_AE);
			return 0;
		}
	} else {
		free_words = SIZE_MAX;
	}

	/* Number of words not containing last byte */
	num_words = (len - 1) >> 2;
	/* Number of bytes in last word transfer */
	num_tail = len - (num_words << 2);
	i = 0;
	if (num_words != 0) {
		mmio_write32(ctx->in.reg_base, FIFO_OTHER_INFO_REG,
				FIFO_INFO_SOP);
		i = 4 * write_words(ctx, buf8, num_words, &free_words);
		if (!free_words) {
			mmio_write32(ctx->csr.reg_base, FIFO_IENABLE_REG,
					FIFO_IENABLE_AE);
			return i;
		}
	}

	mmio_write32(ctx->in.reg_base, FIFO_OTHER_INFO_REG,
			(num_words == 0 ? FIFO_INFO_SOP : 0)
			| FIFO_INFO_EOP
			| FIFO_INFO_EMPTY_SET(4 - num_tail));
	word = 0;
	memcpy(&word, &buf8[i], num_tail);
	mmio_write32(ctx->in.reg_base, FIFO_DATA_REG, ntohl(word));
	return len;
}
