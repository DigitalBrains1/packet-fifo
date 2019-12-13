/* Notes:
 *
 * - Uses lightweight bridge for now
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
#include "mmio.h"
#include "packet-fifo.h"

#define HW_REGS_BASE ( ALT_STM_OFST )
#define HW_REGS_SPAN ( 0x04000000 )
#define HW_REGS_MASK ( HW_REGS_SPAN - 1 )

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
	void *fifo_f2h_base, *fifo_f2h_csr_base, *fifo_h2f_base;
	struct rdfifo_ctx *fifo_f2h_ctx;
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
	fifo_f2h_base = addr_offset_mask(virtual_base,
			ALT_LWFPGASLVS_OFST + FIFO_F2H_OUT_OUT_BASE,
			HW_REGS_MASK);
	fifo_f2h_csr_base = addr_offset_mask(virtual_base,
			ALT_LWFPGASLVS_OFST + FIFO_F2H_OUT_IN_CSR_BASE,
			HW_REGS_MASK);
	fifo_h2f_base = addr_offset_mask(virtual_base,
			ALT_LWFPGASLVS_OFST + FIFO_H2F_IN_BASE,HW_REGS_MASK);

	id = mmio_read32(h2p_sysid_addr, 0);
	printf("%#010x\n", id);

	fifo_f2h_ctx = malloc(rdfifo_ctx_size());
	init_rdfifo(fifo_f2h_ctx, fifo_f2h_base, fifo_f2h_csr_base);

	tmp = 0x12345678;
	for (size_t i = 0; i < 1024; i += 4) {
		memcpy(&outbuf[i], &tmp, 4);
		tmp += 0x1;
	}

	fifo_write(fifo_h2f_base, outbuf, 17);
	while((res = fifo_read(fifo_f2h_ctx)) == FIFO_NEED_MORE) {}
	if (res == 0) {
		uint32_t *p;
		printf("Got packet with size %d bytes:\n",
				fifo_f2h_ctx->numbytes);
		hexdump(fifo_f2h_ctx->buf, fifo_f2h_ctx->numbytes);
		p = &fifo_f2h_ctx->buf[((fifo_f2h_ctx->numbytes-1) >> 2)];
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