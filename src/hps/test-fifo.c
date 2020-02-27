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
#include "avalon_fifo.h"
#include "packet_fifo.h"
#include "uio_helper.h"

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
	int res, ret;
	int fdm, fdu, fdmax;
	fd_set fds;
	struct timeval zero_time = { 0, 0 };
	int loop_count;
	int led_direction;
	int led_mask;
	void *h2p_lw_led_addr;
	void *h2p_sysid_addr;
	void *fifo_f2h_base, *fifo_f2h_csr_base, *fifo_h2f_base;
	struct rdfifo_ctx *fifo_f2h_ctx;
	struct uio_info_t *uio_list, *uio_f2h, *uio;
	uint32_t id, tmp;
	ssize_t len;
	uint8_t outbuf[2048];

	if ((fdm=open("/dev/mem", (O_RDWR | O_SYNC))) == -1) {
		perror("Could not open /dev/mem");
		return 1;
	}

	virtual_base = mmap(NULL, HW_REGS_SPAN, (PROT_READ | PROT_WRITE),
			MAP_SHARED, fdm, HW_REGS_BASE);

	if (virtual_base == MAP_FAILED) {
		perror("mmap() failed");
		close(fdm);
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

	uio_list = uio_find_devices(-1);
	if (!uio_list) {
		fprintf(stderr, "Could not find uio devices.\n");
		return 1;
	}
	uio = uio_list;
	while (uio) {
		uio_get_all_info(uio);
		uio_get_device_attributes(uio);
		uio = uio->next;
	};

	uio_f2h = fifo_uio_by_of_name(uio_list, "fifo-f2h0");
	if (!uio_f2h) {
		fprintf(stderr, "Could not find fifo-f2h0 uio.\n");
		return 1;
	}
	if (fifo_uio_by_of_name(uio_list, "/soc/bridge@0xc0000000/bridge@0x100000000/fifo@0x80000") != uio_f2h) {
		fprintf(stderr, "Got different uio for full name.\n");
		return 1;
	}

	id = mmio_read32(h2p_sysid_addr, 0);
	printf("%#010x\n", id);

	if ((res = init_rdfifo(&fifo_f2h_ctx, uio_f2h, 2048)) != 0) {
		fprintf(stderr, "init_rdfifo error %d\n", res);
		return 1;
	}
	uio_free_info(uio_list);

	fdu = fifo_f2h_ctx->uio_fd;
	fdmax = fdu + 1;
	fifo_f2h_csr_base = fifo_f2h_ctx->csr.reg_base;
	fifo_f2h_base = fifo_f2h_ctx->out.reg_base;

	mmio_write32(fifo_f2h_csr_base, FIFO_EVENT_REG, FIFO_EVENT_ALL);
	mmio_write32(fifo_f2h_csr_base, FIFO_IENABLE_REG, FIFO_IENABLE_ALL);
	/* Flush FIFO */
	for (uint32_t i = mmio_read32(fifo_f2h_csr_base, FIFO_LEVEL_REG);
			i > 0; i--)
		tmp = mmio_read32(fifo_f2h_base, FIFO_DATA_REG);

	tmp = 0x12345678;
	for (size_t i = 0; i < 1024; i += 4) {
		memcpy(&outbuf[i], &tmp, 4);
		tmp += 0x1;
	}


	for (int i = 1; i < 1024; i++) {
		printf("%d\n", i);
		fifo_write(fifo_h2f_base, outbuf, 4);
		FD_ZERO(&fds);
		FD_SET(fdu, &fds);
		select(fdmax, &fds, NULL, NULL, &zero_time);
		if (FD_ISSET(fdu, &fds)) {
			len = read(fdu, &tmp, 4);
			if (len != 4)
				printf("Short read on uio0: %d bytes\n", len);
			printf("Interrupt occurred! %u\n", tmp);
			mmio_write32(fifo_f2h_csr_base, FIFO_EVENT_REG, FIFO_EVENT_ALL);
			mmio_write32(fifo_f2h_csr_base, FIFO_IENABLE_REG, FIFO_IENABLE_ALL);
		}
	}

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

	ret = 0;
	if (munmap(virtual_base, HW_REGS_SPAN) != 0 ) {
		perror("munmap() failed");
		ret = 1;
	}
	if (munmap(fifo_f2h_csr_base , 32) != 0 ) {
		perror("munmap() failed");
		ret = 1;
	}
	close_rdfifo(fifo_f2h_ctx);
	close(fdm);
	return ret;
}
