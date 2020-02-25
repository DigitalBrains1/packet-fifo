/* Notes:
 *
 * - Uses lightweight bridge for now
 */
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

#include "hwlib.h"
#include "socal/socal.h"
#include "socal/hps.h"
#include "socal/alt_gpio.h"
#include "hps_0.h"
#include "mmio.h"
#include "packet_fifo.h"

#define HW_REGS_BASE ( ALT_STM_OFST )
#define HW_REGS_SPAN ( 0x04000000 )
#define HW_REGS_MASK ( HW_REGS_SPAN - 1 )

int
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

void
dump_frame(const uint8_t *buf, const ssize_t len)
{
	for (int i = 0; i < len; i++) {
		printf("%02hhx ", buf[i]);
		if ((i & 15) == 15) {
			printf("\n");
		}
	}
}

void
tun_write(const int fd, const void *data, const ssize_t len)
{
	ssize_t len_w;
	len_w = write(fd, data, len);
	if (len_w < 0) {
		perror("write");
		exit(1);
	}
	if (len_w != len) {
		printf("Incomplete write\n");
		exit(1);
	}
}

int
main(int argc, char *argv[])
{
	uint8_t buf[2048];
	char dev[IFNAMSIZ];
	int memfd,tunfd;
	ssize_t len;
	void *virtual_base;
	int res;
	void *h2p_sysid_addr;
	void *fifo_f2h_base, *fifo_f2h_csr_base, *fifo_h2f_base;
	struct rdfifo_ctx *fifo_f2h_ctx;
	uint32_t id;

	if (argc < 2) {
		printf("Usage\n");
		exit(1);
	}

	if ((memfd=open("/dev/mem", (O_RDWR | O_SYNC))) == -1) {
		perror("Could not open /dev/mem");
		exit(1);
	}

	virtual_base = mmap(NULL, HW_REGS_SPAN, (PROT_READ | PROT_WRITE),
			MAP_SHARED, memfd, HW_REGS_BASE);

	if (virtual_base == MAP_FAILED) {
		perror("mmap() failed");
		close(memfd);
		exit(1);
	}

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

	strncpy(dev, argv[1], IFNAMSIZ);
	if ((tunfd = tun_alloc(dev)) < 0) {
		printf("tun_alloc err");
		exit(1);
	}
	printf("%s\n",dev);
	//ioctl(fd1, TUNSETNOCSUM, 1);

	while (1) {
		len = read(tunfd, buf, sizeof(buf));
		if (len > 0) {
			fifo_write(fifo_h2f_base, buf, len);
		}
		res = fifo_read(fifo_f2h_ctx);
		if (res == 0) {
			tun_write(tunfd, fifo_f2h_ctx->buf,
					fifo_f2h_ctx->numbytes);
		} else if (res != FIFO_NEED_MORE) {
			printf("fifo_read err: %d\n", res);
			exit(1);
		}
	}
}
