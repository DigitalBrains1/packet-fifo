#ifndef FILE_PACKET_FIFO_H
#define FILE_PACKET_FIFO_H
#include <stdlib.h>
#include <stdint.h>

struct
fifo_mapped_reg {
	void *map_base;
	size_t size;
	void *reg_base;
};

struct
rdfifo_ctx {
	struct fifo_mapped_reg out;
	struct fifo_mapped_reg csr;
	int uio_fd;
	size_t numbytes;
	size_t next;
	size_t bufsize;
	size_t word_cnt;
	uint32_t buf[];
};

#define FIFO_GENERAL_ERROR (-1)
#define FIFO_NEED_MORE (-2)
#define FIFO_EVENT_ERROR (-3)
#define FIFO_OVERLONG_ERROR (-4)
#define FIFO_DEV_ERROR (-5)

struct uio_info_t *
fifo_uio_by_of_name(const struct uio_info_t *info, const char *of_name);
extern int
init_rdfifo(struct rdfifo_ctx **ctx, const struct uio_info_t *info,
		size_t maxpkt);
extern void
close_rdfifo(struct rdfifo_ctx *ctx);
int
init_wrfifo(struct fifo_mapped_reg **in, const struct uio_info_t *info);
void
close_wrfifo(struct fifo_mapped_reg *in);
extern int
fifo_read(struct rdfifo_ctx *ctx);
extern void
fifo_write(const struct fifo_mapped_reg *in, const void *buf,
		const size_t len);
#endif // ndef FILE_PACKET_FIFO_H
