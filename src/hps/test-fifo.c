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
#include "mmio.h"
#include "avalon_fifo.h"
#include "packet_fifo.h"
#include "uio_helper.h"

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
	int res;
	int fdmax;
	fd_set fds;
	struct timeval zero_time = { 0, 0 };
	void *f2h_base, *f2h_csr_base;
	struct rdfifo_ctx *f2h_ctx;
	struct fifo_mapped_reg *h2f_ctx;
	struct uio_info_t *uio_list, *uio_f2h, *uio_h2f, *uio;
	uint32_t tmp;
	ssize_t len;
	uint8_t outbuf[2048];

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
	uio_h2f = fifo_uio_by_of_name(uio_list, "fifo-h2f0");
	if (!uio_h2f) {
		fprintf(stderr, "Could not find fifo-h2f0 uio.\n");
		return 1;
	}
	if ((res = init_rdfifo(&f2h_ctx, uio_f2h, 2048)) != 0) {
		fprintf(stderr, "init_rdfifo error %d\n", res);
		return 1;
	}
	if ((res = init_wrfifo(&h2f_ctx, uio_h2f)) != 0) {
		fprintf(stderr, "init_wrfifo error %d\n", res);
		return 1;
	}
	uio_free_info(uio_list);

	fdmax = f2h_ctx->uio_fd + 1;
	f2h_csr_base = f2h_ctx->csr.reg_base;
	f2h_base = f2h_ctx->out.reg_base;

	mmio_write32(f2h_csr_base, FIFO_EVENT_REG, FIFO_EVENT_ALL);
	mmio_write32(f2h_csr_base, FIFO_IENABLE_REG, FIFO_IENABLE_ALL);
	/* Flush FIFO */
	for (uint32_t i = mmio_read32(f2h_csr_base, FIFO_LEVEL_REG);
			i > 0; i--)
		tmp = mmio_read32(f2h_base, FIFO_DATA_REG);

	tmp = 0x12345678;
	for (size_t i = 0; i < 1024; i += 4) {
		memcpy(&outbuf[i], &tmp, 4);
		tmp += 0x1;
	}


	for (int i = 1; i < 1024; i++) {
		printf("%d\n", i);
		fifo_write(h2f_ctx, outbuf, 4);
		FD_ZERO(&fds);
		FD_SET(f2h_ctx->uio_fd, &fds);
		select(fdmax, &fds, NULL, NULL, &zero_time);
		if (FD_ISSET(f2h_ctx->uio_fd, &fds)) {
			len = read(f2h_ctx->uio_fd, &tmp, 4);
			if (len != 4)
				printf("Short read on uio0: %d bytes\n", len);
			printf("Interrupt occurred! %u\n", tmp);
			mmio_write32(f2h_csr_base, FIFO_EVENT_REG, FIFO_EVENT_ALL);
			mmio_write32(f2h_csr_base, FIFO_IENABLE_REG, FIFO_IENABLE_ALL);
		}
	}

	while((res = fifo_read(f2h_ctx)) == FIFO_NEED_MORE) {}
	if (res == 0) {
		uint32_t *p;
		printf("Got packet with size %d bytes:\n",
				f2h_ctx->numbytes);
		hexdump(f2h_ctx->buf, f2h_ctx->numbytes);
		p = &f2h_ctx->buf[((f2h_ctx->numbytes-1) >> 2)];
		printf("Last word: *%p = %#010x\n", (void *) p, *p);
	}

	close_rdfifo(f2h_ctx);
	close_wrfifo(h2f_ctx);
	return 0;
}
