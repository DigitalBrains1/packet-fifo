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
#include "vfy_spec.h"

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

static int
have_f2h_intr(const struct rdfifo_ctx *f2h_ctx)
{
	fd_set fds;
	struct timeval small_time = { 0, 50000 };

	FD_ZERO(&fds);
	FD_SET(f2h_ctx->uio_fd, &fds);
	select(f2h_ctx->uio_fd + 1, &fds, NULL, NULL, &small_time);
	if (FD_ISSET(f2h_ctx->uio_fd, &fds))
		return 1;
	else
		return 0;
}

int
main()
{
	int res;
	void *f2h_base, *f2h_csr_base, *h2f_base;
	struct rdfifo_ctx *f2h_ctx;
	struct wrfifo_ctx *h2f_ctx;
	struct uio_info_t *uio_list, *uio_f2h, *uio_h2f, *uio;
	uint32_t tmp;
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

	f2h_csr_base = f2h_ctx->csr.reg_base;
	f2h_base = f2h_ctx->out.reg_base;
	h2f_base = h2f_ctx->in.reg_base;

	tc_evint_level(f2h_ctx, h2f_ctx);
	tc_race_int_en(f2h_ctx, h2f_ctx);
	tc_evflag_edge(f2h_ctx, h2f_ctx);

	close_rdfifo(f2h_ctx);
	close_wrfifo(h2f_ctx);
	return 0;
}
