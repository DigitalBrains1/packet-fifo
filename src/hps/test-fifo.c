#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <time.h>
#include <unistd.h>

#include "avalon_fifo.h"
#include "mmio.h"
#include "packet_fifo.h"
#include "uio_helper.h"
#include "vfy_spec.h"

static void
hexdump(void *data, size_t len) __attribute__((unused));
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
have_intr(int fd)
{
	fd_set fds;
	struct timeval small_time = { 0, 50000 };

	FD_ZERO(&fds);
	FD_SET(fd, &fds);
	select(fd + 1, &fds, NULL, NULL, &small_time);
	if (FD_ISSET(fd, &fds))
		return 1;
	else
		return 0;
}

static int
vfy_no_intr(int fd)
{
	printf("Interrupt: ");
	if (have_intr(fd)) {
		printf("yes - UNEXPECTED.\n");
		return 1;
	} else {
		printf("no.\n");
		return 0;
	}
}

static int
vfy_intr(int fd)
{
	printf("Interrupt: ");
	if (have_intr(fd)) {
		printf("yes.\n");
		return 0;
	} else {
		printf("no - UNEXPECTED.\n");
		return 1;
	}
}

/*
 * The FIFOs have the following depths:
 * h2f_in: 512
 * (h2f_out: 8)
 * (f2h_in: 8)
 * h2f_out: 512
 *
 * Only h2f_in and h2f_out can be monitored.
 */
static int
analyze_backpr(struct rdfifo_ctx *f2h_ctx, const struct wrfifo_ctx *h2f_ctx)
{
	size_t rdlevel, wrlevel;
	size_t last_rdlevel = 0;
	size_t last_wrlevel = 0;
	size_t mono_rdlevel = 0;
	size_t mono_wrlevel = 0;
	size_t when_rdlevel = 0;
	size_t when_wrlevel = 0;
	uint32_t sendpkt, data;
	uint32_t seen = 0;
	int res;

	printf("Starting backpressure analysis.\n");
	if (!h2f_ctx->csr.reg_base) {
		fprintf(stderr, "No CSR for fifo-h2f0!\n");
		return 1;
	}
	rdlevel = mmio_read32(f2h_ctx->csr.reg_base, FIFO_LEVEL_REG);
	wrlevel = mmio_read32(h2f_ctx->csr.reg_base, FIFO_LEVEL_REG);
	if (rdlevel || wrlevel) {
		fprintf(stderr, "FIFO's not empty at start!\n");
		return 1;
	}

	for (sendpkt=1; sendpkt <= 1050; sendpkt++) {
		struct timespec delay = { 0, 50000 };

		fifo_write(h2f_ctx, &sendpkt, 4);
		while(nanosleep(&delay, &delay)) {
			if (errno != EINTR) {
				perror("analyze_backpr() nanosleep()");
				exit(1);
			}
		}
		rdlevel = mmio_read32(f2h_ctx->csr.reg_base, FIFO_LEVEL_REG);
		wrlevel = mmio_read32(h2f_ctx->csr.reg_base, FIFO_LEVEL_REG);
		if (rdlevel < last_rdlevel) {
			mono_rdlevel = rdlevel;
			when_rdlevel = 0;
		} else if (!when_rdlevel && rdlevel > last_rdlevel) {
			when_rdlevel = sendpkt;
		}
		if (wrlevel < last_wrlevel) {
			mono_wrlevel = wrlevel;
			when_wrlevel = 0;
		} else if (!when_wrlevel && wrlevel > last_wrlevel) {
			when_wrlevel = sendpkt;
		}
		last_rdlevel = rdlevel;
		last_wrlevel = wrlevel;
		if (wrlevel == (size_t) h2f_ctx->depth) {
			sendpkt++;
			break;
		}
	}
	sendpkt--;
	printf("Sent %d packets.\n", sendpkt);
	printf("rdfifo monotonically increasing from %d (packet %d) to %d.\n",
			mono_rdlevel, when_rdlevel, last_rdlevel);
	printf("wrfifo monotonically increasing from %d (packet %d) to %d.\n",
			mono_wrlevel, when_wrlevel, last_wrlevel);

	while (1) {
		res = fifo_read(f2h_ctx);
		if (res == 0) {
			if (f2h_ctx->numbytes != 4) {
				fprintf(stderr, "analyze_backpr(): Received "
						"size != 4. Abort.\n");
				exit(1);
			}
			memcpy(&data, f2h_ctx->buf, 4);
			seen++;
			if (data != seen) {
				printf("Data gap %d-%d\n", seen, data-1);
				seen = data;
			}
		} else if (res == FIFO_NEED_MORE) {
			if (!have_intr(f2h_ctx->uio_fd)) {
				break;
			}
		} else {
			fprintf(stderr, "analyze_backpr(): fifo_read() error "
					"%d.\n", res);
			exit(1);
		}
	}
	if (seen != sendpkt)
		printf("Data gap %d-%d\n", seen+1, sendpkt);
	return 0;
}

static int
vfy_fifo_write(const char *name, ssize_t expect, const struct wrfifo_ctx *ctx,
		const void *buf, size_t len)
{
	ssize_t res;

	res = fifo_write(ctx, buf, len);
	printf("fifo_write() %s: %d", name, res);
	if (res != expect) {
		printf(" - ERROR!\n");
		return 1;
	} else {
		putchar('\n');
		return 0;
	}
}

static void
dump_ae(const struct wrfifo_ctx *ctx)
{
	printf("AE status: %s\n", mmio_read32(ctx->csr.reg_base,
			FIFO_STATUS_REG) & FIFO_STATUS_AE ? "set" : "unset");
	printf("AE event: %s\n", mmio_read32(ctx->csr.reg_base,
			FIFO_EVENT_REG) & FIFO_EVENT_AE ? "set" : "unset");
	printf("AE ienable: %s\n", mmio_read32(ctx->csr.reg_base,
			FIFO_IENABLE_REG) & FIFO_IENABLE_AE ? "set" :
			"unset");
}

static int
test_wrfifo(struct rdfifo_ctx *f2h_ctx, const struct wrfifo_ctx *h2f_ctx)
{
	ssize_t thresh;
	uint32_t sendpkt[] = {
			0xFEEDFACE, 0xDEADBEEF, 0xC0DEBABE, 0xC0DED00D };

	if (h2f_ctx->mode != FIFO_INTR) {
		fprintf(stderr, "h2f mode != FIFO_INTR.\n");
		return 1;
	}
	thresh = set_wrfifo_thresh(h2f_ctx, h2f_ctx->depth - 1);
	if (thresh != h2f_ctx->depth -1) {
		fprintf(stderr, "Unexpected wr_thresh %d.\n", thresh);
		return 1;
	}
	if (have_intr(h2f_ctx->uio_fd))
		printf("Note: clearing pending interrupt on startup.\n");
	dump_ae(h2f_ctx);
	printf("Filling up FIFOs...\n");
	while(((ssize_t) mmio_read32(h2f_ctx->csr.reg_base, FIFO_LEVEL_REG))
			!= h2f_ctx->depth)
		fifo_write(h2f_ctx, &sendpkt, 4);
	dump_ae(h2f_ctx);
	vfy_fifo_write("when full", 0, h2f_ctx, &sendpkt, 4);
	dump_ae(h2f_ctx);
	vfy_no_intr(h2f_ctx->uio_fd);
	printf("Free room: %d.\n", h2f_ctx->depth -
			mmio_read32(h2f_ctx->csr.reg_base, FIFO_LEVEL_REG));
	fifo_read(f2h_ctx);
	printf("Free room: %d.\n", h2f_ctx->depth -
			mmio_read32(h2f_ctx->csr.reg_base, FIFO_LEVEL_REG));
	dump_ae(h2f_ctx);
	vfy_intr(h2f_ctx->uio_fd);
	dump_ae(h2f_ctx);
	printf("Filling up FIFOs...\n");
	while(((ssize_t) mmio_read32(h2f_ctx->csr.reg_base, FIFO_LEVEL_REG))
			!= h2f_ctx->depth - 1)
		fifo_write(h2f_ctx, &sendpkt, 4);
	vfy_fifo_write("tail over-limit", 4, h2f_ctx, &sendpkt, 8);
	fifo_read(f2h_ctx);
	fifo_read(f2h_ctx);
	printf("Free room: %d.\n", h2f_ctx->depth -
			mmio_read32(h2f_ctx->csr.reg_base, FIFO_LEVEL_REG));
	vfy_intr(h2f_ctx->uio_fd);
	printf("Filling up FIFOs...\n");
	while(((ssize_t) mmio_read32(h2f_ctx->csr.reg_base, FIFO_LEVEL_REG))
			!= h2f_ctx->depth - 2)
		fifo_write(h2f_ctx, &sendpkt, 4);
	vfy_fifo_write("short write_word()", 8, h2f_ctx, &sendpkt, 16);
	printf("Empty FIFOs...\n");
	while(mmio_read32(f2h_ctx->csr.reg_base, FIFO_LEVEL_REG))
		mmio_read32(f2h_ctx->out.reg_base, FIFO_DATA_REG);
	dump_ae(h2f_ctx);
	return 0;
}

int
main()
{
	int res;
	struct rdfifo_ctx *f2h_ctx;
	struct wrfifo_ctx *h2f_ctx;
	struct uio_info_t *uio_list, *uio_f2h, *uio_h2f, *uio;

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

	tc_evint_level(f2h_ctx, h2f_ctx);
	tc_race_int_en(f2h_ctx, h2f_ctx);
	tc_evflag_edge(f2h_ctx, h2f_ctx);

	analyze_backpr(f2h_ctx, h2f_ctx);
	test_wrfifo(f2h_ctx, h2f_ctx);

	close_rdfifo(f2h_ctx);
	close_wrfifo(h2f_ctx);
	return 0;
}
