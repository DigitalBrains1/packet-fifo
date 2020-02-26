#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <string.h>
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

size_t
rdfifo_ctx_size()
{
	return sizeof(struct rdfifo_ctx);
}

int
init_rdfifo(struct rdfifo_ctx *ctx, void *base, void *csr_base)
{
	ctx->base = base;
	ctx->csr_base = csr_base;
	ctx->next = 0;
	ctx->bufsize = 512;
	ctx->word_cnt = 0;
#if 0
	/* Flush FIFO */
	for (uint32_t i = mmio_read32(csr_base, FIFO_LEVEL_REG); i > 0; i--) {
		uint32_t tmp;
		tmp = mmio_read32(base, FIFO_DATA_REG);
		ctx->word_cnt++;
		printf("Init flush read fifo, read: %6d %#010x\n",
				ctx->word_cnt, tmp);
	}
#endif
	/* Clear events */
	mmio_write32(csr_base, FIFO_EVENT_REG, FIFO_EVENT_ALL);
	if ((mmio_read32(csr_base, FIFO_EVENT_REG) & FIFO_EVENT_ALL) != 0) {
		return FIFO_EVENT_ERROR;
	}

	return 0;
}

int
fifo_read(struct rdfifo_ctx *ctx)
{
	uint32_t tmp, num_elems;
	uint32_t other = 0;

	num_elems = mmio_read32(ctx->csr_base, FIFO_LEVEL_REG);
	if (num_elems == 0)
		return FIFO_NEED_MORE;
	tmp = mmio_read32(ctx->base, FIFO_DATA_REG);
	other = mmio_read32(ctx->base, FIFO_OTHER_INFO_REG);
	ctx->word_cnt++;
	num_elems--;

	if (ctx->next == 0) {
		/* Flush until start of packet */
		while (!(other & FIFO_INFO_SOP) && (num_elems != 0)) {
			printf("Interpacket flush read fifo, read: "
					"%6d %#010x\n", ctx->word_cnt, tmp);
			tmp = mmio_read32(ctx->base, FIFO_DATA_REG);
			other = mmio_read32(ctx->base,
					FIFO_OTHER_INFO_REG);
			ctx->word_cnt++;
			if (--num_elems == 0)
				num_elems = mmio_read32(ctx->csr_base,
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
		ctx->buf[ctx->next++] = htonl(mmio_read32(ctx->base,
				FIFO_DATA_REG));
		other = mmio_read32(ctx->base, FIFO_OTHER_INFO_REG);
		ctx->word_cnt++;
		if (--num_elems == 0)
			num_elems = mmio_read32(ctx->csr_base,
					FIFO_LEVEL_REG);
	}

	if (!(other & FIFO_INFO_EOP))
		return FIFO_NEED_MORE;

	ctx->numbytes = ctx->next * 4 - FIFO_INFO_EMPTY_GET(other);
	ctx->next = 0;
	return 0;
}

void
fifo_write(void *fifo_base, const void *buf, const size_t len)
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
		mmio_write32(fifo_base, FIFO_OTHER_INFO_REG, FIFO_INFO_SOP);
		for (i = 0; i < num_words * 4; i += 4) {
			memcpy(&word, &buf8[i], 4);
			mmio_write32(fifo_base, FIFO_DATA_REG, ntohl(word));
		}
	}

	mmio_write32(fifo_base, FIFO_OTHER_INFO_REG,
			(num_words == 0 ? FIFO_INFO_SOP : 0)
			| FIFO_INFO_EOP
			| FIFO_INFO_EMPTY_SET(4 - num_tail));
	word = 0;
	memcpy(&word, &buf8[i], num_tail);
	mmio_write32(fifo_base, FIFO_DATA_REG, ntohl(word));
}
