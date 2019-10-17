/* Notes:
 *
 * - Uses lightweight bridge for now
 *
 * - TODO: Should there even be ntohl()/htonl()? FIFO_LEVEL_REG could count to
 *   511 without issues, that seems to indicate it is already handled at the
 *   hardware level, right? It should become clear once there is CλaSH code on
 *   the other side of the FIFO.
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <arpa/inet.h>
#include <string.h>
#include <sys/mman.h>
#include "hwlib.h"
#include "socal/socal.h"
#include "socal/hps.h"
#include "socal/alt_gpio.h"
#include "hps_0.h"

#define HW_REGS_BASE ( ALT_STM_OFST )
#define HW_REGS_SPAN ( 0x04000000 )
#define HW_REGS_MASK ( HW_REGS_SPAN - 1 )

static inline void *
addr_offset(void *base, size_t offset)
{
	return (((char *) base) + offset);
}

static inline void *
addr_offset_mask(void *base, size_t offset, size_t mask)
{
	return (((char *) base) + (offset & mask));
}

static inline void
mmio_write32(void *base, size_t offset, uint32_t data)
{
	*(volatile uint32_t *) addr_offset(base, offset) = data;
}

static inline uint32_t
mmio_read32(void *base, size_t offset)
{
	return *(volatile uint32_t *) addr_offset(base, offset);
}

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

struct infifo_ctx {
	void *base;
	void *csr_base;
	size_t numbytes;
	size_t next;
	size_t bufsize;
	uint32_t buf[512];
};

#define FIFO_NEED_MORE (-2)
#define FIFO_EVENT_ERROR (-3)
#define FIFO_OVERLONG_ERROR (-4)

size_t
infifo_ctx_size()
{
	return sizeof(struct infifo_ctx);
}

int
init_infifo(struct infifo_ctx *ctx, void *base, void *csr_base)
{
	ctx->base = base;
	ctx->csr_base = csr_base;
	ctx->next = 0;
	ctx->bufsize = 512;

	/* Flush FIFO */
	for (uint32_t i = mmio_read32(csr_base, FIFO_LEVEL_REG); i > 0; i--) {
		mmio_read32(base, FIFO_DATA_REG);
	}

	/* Clear events */
	mmio_write32(csr_base, FIFO_EVENT_REG, FIFO_EVENT_ALL);
	if ((mmio_read32(csr_base, FIFO_EVENT_REG) & FIFO_EVENT_ALL) != 0) {
		return FIFO_EVENT_ERROR;
	}

	return 0;
}

int
fifo_read(struct infifo_ctx *ctx)
{
	uint32_t tmp, num_elems;
	uint32_t other = 0;

	num_elems = mmio_read32(ctx->csr_base, FIFO_LEVEL_REG);
	if (num_elems == 0)
		return FIFO_NEED_MORE;
	tmp = mmio_read32(ctx->base, FIFO_DATA_REG);
	other = mmio_read32(ctx->base, FIFO_OTHER_INFO_REG);
	num_elems--;

	if (ctx->next == 0) {
		/* Flush until start of packet */
		while (!(other & FIFO_INFO_SOP) && (num_elems != 0)) {
			tmp = mmio_read32(ctx->base, FIFO_DATA_REG);
			other = mmio_read32(ctx->base,
					FIFO_OTHER_INFO_REG);
			if (--num_elems == 0)
				num_elems = mmio_read32(ctx->csr_base,
						FIFO_LEVEL_REG);
		}
		if (!(other & FIFO_INFO_SOP))
			return FIFO_NEED_MORE;
	}

	ctx->buf[ctx->next++] = ntohl(tmp);
	while (!(other & FIFO_INFO_EOP) && (num_elems != 0)) {
		if (ctx->next == ctx->bufsize) {
			ctx->next = 0;
			return FIFO_OVERLONG_ERROR;
		}
		ctx->buf[ctx->next++] = ntohl(mmio_read32(ctx->base,
				FIFO_DATA_REG));
		other = mmio_read32(ctx->base, FIFO_OTHER_INFO_REG);
		if (--num_elems == 0)
			num_elems = mmio_read32(ctx->csr_base,
					FIFO_LEVEL_REG);
	}

	if (!(other & FIFO_INFO_EOP))
		return FIFO_NEED_MORE;

	ctx->numbytes = ctx->next * 4 - FIFO_INFO_EMPTY_GET(other);
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
			mmio_write32(fifo_base, FIFO_DATA_REG, htonl(word));
		}
	}

	mmio_write32(fifo_base, FIFO_OTHER_INFO_REG,
			(num_words == 0 ? FIFO_INFO_SOP : 0)
			| FIFO_INFO_EOP
			| FIFO_INFO_EMPTY_SET(4 - num_tail));
	word = 0;
	memcpy(&word, &buf8[i], num_tail);
	mmio_write32(fifo_base, FIFO_DATA_REG, htonl(word));
}

static void
hexdump(void *data, size_t len)
{
	uint8_t *data8 = data;

	for (size_t i = 0; i < len; i++) {
		printf("%02hhx ", data8[i]);
		if ((i & 15) == 15)
			printf("\n");
	}
	if ((len & 15) != 0)
		printf("\n");
}

int
main()
{
	void *virtual_base;
	int res;
	int fd;
	int loop_count;
	int led_direction;
	int led_mask;
	void *h2p_lw_led_addr;
	void *h2p_sysid_addr;
	void *infifo_base, *infifo_csr_base, *outfifo_base;
	struct infifo_ctx *infifo_ctx;
	uint32_t id, tmp;
	uint8_t outbuf[2048];

	if ((fd=open("/dev/mem", (O_RDWR | O_SYNC))) == -1) {
		perror("Could not open /dev/mem");
		return 1;
	}

	virtual_base = mmap(NULL, HW_REGS_SPAN, (PROT_READ | PROT_WRITE),
			MAP_SHARED, fd, HW_REGS_BASE);

	if (virtual_base == MAP_FAILED) {
		perror("mmap() failed");
		close(fd);
		return 1;
	}

	h2p_lw_led_addr = addr_offset_mask(virtual_base,
			ALT_LWFPGASLVS_OFST + LED_PIO_BASE,HW_REGS_MASK);
	h2p_sysid_addr = addr_offset_mask(virtual_base,
			ALT_LWFPGASLVS_OFST + SYSID_QSYS_BASE,HW_REGS_MASK);
	infifo_base = addr_offset_mask(virtual_base,
			ALT_LWFPGASLVS_OFST + FIFO_1_OUT_BASE,HW_REGS_MASK);
	infifo_csr_base = addr_offset_mask(virtual_base,
			ALT_LWFPGASLVS_OFST + FIFO_1_IN_CSR_BASE,HW_REGS_MASK);
	outfifo_base = addr_offset_mask(virtual_base,
			ALT_LWFPGASLVS_OFST + FIFO_0_BASE,HW_REGS_MASK);

	id = mmio_read32(h2p_sysid_addr, 0);
	printf("%#010x\n", id);

	infifo_ctx = malloc(infifo_ctx_size());
	init_infifo(infifo_ctx, infifo_base, infifo_csr_base);

	tmp = 0x12345678;
	for (size_t i = 0; i < 1024; i += 4) {
		memcpy(&outbuf[i], &tmp, 4);
		tmp += 0x1;
	}

	fifo_write(outfifo_base, outbuf, 1021);
	while((res = fifo_read(infifo_ctx)) == FIFO_NEED_MORE) {}
	if (res == 0) {
		uint32_t *p;
		printf("Got packet with size %d bytes:\n",
				infifo_ctx->numbytes);
		hexdump(infifo_ctx->buf, infifo_ctx->numbytes);
		p = &infifo_ctx->buf[((infifo_ctx->numbytes-1) >> 2)];
		printf("Last word: *%p = %#010x\n", (void *) p, *p);
	}

	// toggle the LEDs a bit

	loop_count = 0;
	led_mask = 0x01;
	led_direction = 0; // 0: left to right direction
	while (loop_count < 60) {
		// control led
		mmio_write32(h2p_lw_led_addr, 0, led_mask);
		// wait 100ms
		usleep (100*1000);
		// update led mask
		if (led_direction == 0) {
			led_mask <<= 1;
			if (led_mask == (0x01 << (LED_PIO_DATA_WIDTH-1)))
				led_direction = 1;
		}else{
			led_mask >>= 1;
			if (led_mask == 0x01){
				led_direction = 0;
				loop_count++;
			}
		}
	}

	// clean up our memory mapping and exit
	if (munmap(virtual_base, HW_REGS_SPAN) != 0 ) {
		perror("munmap() failed");
		close(fd);
		return 1;
	}
	close(fd);
	return 0;
}
