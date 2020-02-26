#ifndef FILE_PACKET_FIFO_H
#define FILE_PACKET_FIFO_H
#include <stdlib.h>
#include <stdint.h>

struct
rdfifo_ctx {
	void *base;
	void *csr_base;
	size_t numbytes;
	size_t next;
	size_t bufsize;
	size_t word_cnt;
	uint32_t buf[512];
};

#define FIFO_NEED_MORE (-2)
#define FIFO_EVENT_ERROR (-3)
#define FIFO_OVERLONG_ERROR (-4)

struct uio_info_t *
fifo_uio_by_of_name(const struct uio_info_t *info, const char *of_name);
extern size_t
rdfifo_ctx_size();
extern int
init_rdfifo(struct rdfifo_ctx *ctx, void *base, void *csr_base);
extern int
fifo_read(struct rdfifo_ctx *ctx);
extern void
fifo_write(void *fifo_base, const void *buf, const size_t len);

#endif // ndef FILE_PACKET_FIFO_H
