#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <fcntl.h>
#include <arpa/inet.h>
#include <string.h>
#include <sys/mman.h>
#include "mmio.h"
#include "avalon_fifo.h"
#include "packet_fifo.h"
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
init_wrfifo(struct fifo_mapped_reg **in, const struct uio_info_t *info)
{
	int fd;
	char uio_dev_n[16];
	int ret;
	size_t i;

	*in = malloc(sizeof(**in));
	if (!*in) {
		ret = FIFO_GENERAL_ERROR;
		goto init_wr_out_err;
	}

	ret=FIFO_DEV_ERROR;
	sprintf(uio_dev_n, "/dev/uio%d", info->uio_num);
	if ((fd=open(uio_dev_n, O_RDWR)) == -1)
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
	(*in)->map_base = mmap(NULL, info->maps[i].size,
			(PROT_READ | PROT_WRITE), MAP_SHARED, fd,
			i * getpagesize());
	if ((*in)->map_base == MAP_FAILED)
		goto init_wr_out_close;
	(*in)->size = info->maps[i].size;
	(*in)->reg_base = addr_offset((*in)->map_base, info->maps[i].offset);
	close(fd);

	return 0;
init_wr_out_close:
	close(fd);
init_wr_out_free_ctx:
	free(*in);
init_wr_out_err:
	return ret;
}

void
close_wrfifo(struct fifo_mapped_reg *in)
{
	munmap(in->map_base, in->size);
	free(in);
}

int
fifo_read(struct rdfifo_ctx *ctx)
{
	uint32_t tmp, num_elems;
	uint32_t other = 0;

	num_elems = mmio_read32(ctx->csr.reg_base, FIFO_LEVEL_REG);
	if (num_elems == 0)
		return FIFO_NEED_MORE;
	tmp = mmio_read32(ctx->out.reg_base, FIFO_DATA_REG);
	other = mmio_read32(ctx->out.reg_base, FIFO_OTHER_INFO_REG);
	ctx->word_cnt++;
	num_elems--;

	if (ctx->next == 0) {
		/* Flush until start of packet */
		while (!(other & FIFO_INFO_SOP) && (num_elems != 0)) {
			tmp = mmio_read32(ctx->out.reg_base, FIFO_DATA_REG);
			other = mmio_read32(ctx->out.reg_base,
					FIFO_OTHER_INFO_REG);
			ctx->word_cnt++;
			if (--num_elems == 0)
				num_elems = mmio_read32(ctx->csr.reg_base,
						FIFO_LEVEL_REG);
		}
		if (!(other & FIFO_INFO_SOP))
			return FIFO_NEED_MORE;
	}

	ctx->buf[ctx->next++] = htonl(tmp);
	while (!(other & FIFO_INFO_EOP) && (num_elems != 0)) {
		if (ctx->next == ctx->bufsize) {
			ctx->next = 0;
			return FIFO_OVERLONG_ERROR;
		}
		ctx->buf[ctx->next++] = htonl(mmio_read32(ctx->out.reg_base,
				FIFO_DATA_REG));
		other = mmio_read32(ctx->out.reg_base, FIFO_OTHER_INFO_REG);
		ctx->word_cnt++;
		if (--num_elems == 0)
			num_elems = mmio_read32(ctx->csr.reg_base,
					FIFO_LEVEL_REG);
	}

	if (!(other & FIFO_INFO_EOP))
		return FIFO_NEED_MORE;

	ctx->numbytes = ctx->next * 4 - FIFO_INFO_EMPTY_GET(other);
	ctx->next = 0;
	return 0;
}

void
fifo_write(const struct fifo_mapped_reg *in, const void *buf, size_t len)
{
	const uint8_t *buf8 = (const uint8_t *) buf;
	uint32_t word;
	size_t num_words, num_tail, i;

	/* Number of words not containing last byte */
	num_words = (len - 1) >> 2;
	/* Number of bytes in last word transfer */
	num_tail = len - (num_words << 2);
	i = 0;
	if (num_words != 0) {
		mmio_write32(in->reg_base, FIFO_OTHER_INFO_REG,
				FIFO_INFO_SOP);
		for (i = 0; i < num_words * 4; i += 4) {
			memcpy(&word, &buf8[i], 4);
			mmio_write32(in->reg_base, FIFO_DATA_REG,
					ntohl(word));
		}
	}

	mmio_write32(in->reg_base, FIFO_OTHER_INFO_REG,
			(num_words == 0 ? FIFO_INFO_SOP : 0)
			| FIFO_INFO_EOP
			| FIFO_INFO_EMPTY_SET(4 - num_tail));
	word = 0;
	memcpy(&word, &buf8[i], num_tail);
	mmio_write32(in->reg_base, FIFO_DATA_REG, ntohl(word));
}
