#ifndef FILE_PACKET_FIFO_H
#define FILE_PACKET_FIFO_H
#include <stdlib.h>
#include <stdint.h>

#define FIFO_DATA_REG                          0
#define FIFO_OTHER_INFO_REG                    4

#define FIFO_INFO_SOP		1
#define FIFO_INFO_EOP		2
#define FIFO_INFO_EMPTY_SHIFT	2
#define FIFO_INFO_EMPTY_MASK	(0x1f << FIFO_INFO_EMPTY_SHIFT)

#define FIFO_INFO_EMPTY_GET(reg) \
	(((reg) & FIFO_INFO_EMPTY_MASK) >> FIFO_INFO_EMPTY_SHIFT)
#define FIFO_INFO_EMPTY_SET(val) \
	(((val) << FIFO_INFO_EMPTY_SHIFT) & FIFO_INFO_EMPTY_MASK)

#define FIFO_LEVEL_REG                         0
#define FIFO_STATUS_REG                        4
#define FIFO_EVENT_REG                         8
#define FIFO_IENABLE_REG                       12
#define FIFO_ALMOSTFULL_REG                    16
#define FIFO_ALMOSTEMPTY_REG                   20

#define FIFO_EVENT_F    (0x01)
#define FIFO_EVENT_E    (0x02)
#define FIFO_EVENT_AF   (0x04)
#define FIFO_EVENT_AE   (0x08)
#define FIFO_EVENT_OVF  (0x10)
#define FIFO_EVENT_UDF  (0x20)
#define FIFO_EVENT_ALL  (0x3F)

#define FIFO_STATUS_F    (0x01)
#define FIFO_STATUS_E    (0x02)
#define FIFO_STATUS_AF   (0x04)
#define FIFO_STATUS_AE   (0x08)
#define FIFO_STATUS_OVF  (0x10)
#define FIFO_STATUS_UDF  (0x20)
#define FIFO_STATUS_ALL  (0x3F)

#define FIFO_IENABLE_F    (0x01)
#define FIFO_IENABLE_E    (0x02)
#define FIFO_IENABLE_AF   (0x04)
#define FIFO_IENABLE_AE   (0x08)
#define FIFO_IENABLE_OVF  (0x10)
#define FIFO_IENABLE_UDF  (0x20)
#define FIFO_IENABLE_ALL  (0x3F)

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

extern size_t
rdfifo_ctx_size();
extern int
init_rdfifo(struct rdfifo_ctx *ctx, void *base, void *csr_base);
extern int
fifo_read(struct rdfifo_ctx *ctx);
extern void
fifo_write(void *fifo_base, const void *buf, const size_t len);

#endif // ndef FILE_PACKET_FIFO_H
