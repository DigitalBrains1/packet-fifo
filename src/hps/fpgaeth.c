#define _GNU_SOURCE 1
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <arpa/inet.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/types.h>

#include <linux/if.h>
#include <linux/if_tun.h>

#include "mmio.h"
#include "avalon_fifo.h"
#include "packet_fifo.h"
#include "uio_helper.h"

static int
tun_alloc(char *dev)
{
	struct ifreq ifr;
	int fd, err;

	if ((fd = open("/dev/net/tun", O_RDWR | O_NONBLOCK)) < 0) {
		perror("Open tun");
		return fd;
	}

	memset(&ifr, 0, sizeof(ifr));

	/* Flags: IFF_TUN   - TUN device (no Ethernet headers)
	 *        IFF_TAP   - TAP device
	 *
	 *        IFF_NO_PI - Do not provide packet information
	 */
	ifr.ifr_flags = IFF_TAP | IFF_NO_PI;
	if (*dev)
		strncpy(ifr.ifr_name, dev, IFNAMSIZ);
	if ((err = ioctl(fd, TUNSETIFF, (void *) &ifr)) < 0) {
		perror("ioctl");
		close(fd);
		return err;
	}
	strcpy(dev, ifr.ifr_name);
	return fd;
}

static void
dump_frame(const uint8_t *buf, ssize_t len) __attribute__((unused));
static void
dump_frame(const uint8_t *buf, ssize_t len)
{
	for (int i = 0; i < len; i++) {
		printf("%02hhx ", buf[i]);
		if ((i & 15) == 15) {
			printf("\n");
		}
	}
}

static int
handle_f2h_packet(struct rdfifo_ctx *f2h_ctx, const int tunfd)
{
	int res;
	ssize_t len_w;

	res = fifo_read(f2h_ctx);
	if (res == 0) {
		len_w = write(tunfd, f2h_ctx->buf,
				f2h_ctx->numbytes);
		if (len_w < 0) {
			perror("write");
			exit(1);
		}
		if (len_w != (ssize_t) f2h_ctx->numbytes) {
			printf("Incomplete tunnel write\n");
			exit(1);
		}
		return 1;
	} else if (res == FIFO_NEED_MORE) {
		return 0;
	} else {
		printf("fifo_read err: %d\n", res);
		exit(1);
	}
}

static int
handle_tun_packet(const int tunfd, struct fifo_mapped_reg *h2f_ctx)
{
	uint8_t buf[2048];
	ssize_t len;

	len = read(tunfd, buf, sizeof(buf));
	if (len > 0) {
		fifo_write(h2f_ctx, buf, len);
		return 1;
	} else {
		return 0;
	}
}

int
main(int argc, char *argv[])
{
	char dev[IFNAMSIZ];
	struct rdfifo_ctx *f2h_ctx;
	struct fifo_mapped_reg *h2f_ctx;
	struct uio_info_t *uio_list, *uio_f2h, *uio_h2f, *uio;
	int res, read_f2h, read_tun;
	fd_set fds;
	int tunfd, fdmax;
	uint32_t tmp;

	if (argc < 2) {
		printf("Usage\n");
		exit(1);
	}

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

	strncpy(dev, argv[1], IFNAMSIZ);
	dev[IFNAMSIZ-1] = '\0';
	if ((tunfd = tun_alloc(dev)) < 0) {
		printf("tun_alloc err");
		exit(1);
	}
	printf("%s\n",dev);
	//ioctl(fd1, TUNSETNOCSUM, 1);

	fdmax = (tunfd > f2h_ctx->uio_fd ? tunfd : f2h_ctx->uio_fd) + 1;

	read_f2h = 1;
	read_tun = 1;
	while (1) {
		if (read_f2h)
			read_f2h = handle_f2h_packet(f2h_ctx, tunfd);
		if (read_tun)
			read_tun = handle_tun_packet(tunfd, h2f_ctx);
		if (!read_f2h && !read_tun) {
			FD_ZERO(&fds);
			FD_SET(tunfd, &fds);
			FD_SET(f2h_ctx->uio_fd, &fds);
			select(fdmax, &fds, NULL, NULL, NULL);
			if (FD_ISSET(f2h_ctx->uio_fd, &fds)) {
				read(f2h_ctx->uio_fd, &tmp, 4);
				read_f2h = 1;
			}
			if (FD_ISSET(tunfd, &fds))
				read_tun = 1;
		}
	}
}
