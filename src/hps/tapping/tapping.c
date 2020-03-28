#define _GNU_SOURCE 1
#include <arpa/inet.h>
#include <fcntl.h>
#include <net/ethernet.h>
#include <netinet/ether.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <netinet/ip_icmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#include <linux/if.h>
#include <linux/if_tun.h>

const struct ether_addr
my_ether_addr =
	{	{ 0x52, 0x54, 0x00
		, 0xeb, 0x9b, 0xd0 } };
struct in_addr
my_ip_addr;

int
tun_alloc(char *dev)
{
	struct ifreq ifr;
	int fd, err;

	if ((fd = open("/dev/net/tun", O_RDWR)) < 0) {
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
parse_frame(const uint8_t *buf, const ssize_t len, int rep_fd)
{
	const uint8_t *cur;
	ssize_t left;
	const struct ether_header *eth_h;
	const struct ether_addr *eth_shost, *eth_dhost;
	const struct ip *ip_hdr;
	const struct in_addr *ip_src, *ip_dst;
	const struct icmp *icmp_msg;
	uint8_t rep[1600];
	struct ether_header *rep_eth_h;
	struct ip *rep_ip_hdr;
	struct icmp *rep_icmp_msg;
	uint32_t cksum;
	int len_w;

	printf("\n\n\n");
	if (len < ETHER_HDR_LEN) {
		printf("Runt frame, not parsing:\n");
		dump_frame(buf, len);
		return;
	}
	cur = buf;
	left = len;
	eth_h = (const struct ether_header*) cur;
	eth_shost = (const struct ether_addr*) eth_h->ether_shost;
	eth_dhost = (const struct ether_addr*) eth_h->ether_dhost;
	printf("%s -> ", ether_ntoa(eth_shost));
	printf("%s", ether_ntoa(eth_dhost));

	if (memcmp(eth_dhost, &my_ether_addr, ETH_ALEN)) {
		printf("\n");
		return;
	}
	printf(" !");
	if (eth_h->ether_type != htons(ETHERTYPE_IP)) {
		printf("\n");
		return;
	}
	printf(" IPv4\n");

	cur += ETHER_HDR_LEN;
	left -= ETHER_HDR_LEN;
	if (left < 20) {
		// Minimum IPv4 packet is a 20-byte header with no data
		printf("\nRunt packet, not parsing:\n");
		dump_frame(buf, len);
		return;
	}
	ip_hdr = (const struct ip*) cur;
	ip_src = &ip_hdr->ip_src;
	ip_dst = &ip_hdr->ip_dst;
	printf("%s -> ", inet_ntoa(*ip_src));
	printf("%s", inet_ntoa(*ip_dst));
	if (ip_dst->s_addr != my_ip_addr.s_addr) {
		printf("\n");
		return;
	}
	printf(" !");

	if (ip_hdr->ip_p != IPPROTO_ICMP) {
		printf("\n");
		return;
	}
	printf(" ICMP");

	cur += ip_hdr->ip_hl << 2;
	left -= ip_hdr->ip_hl << 2;
	if (left < ICMP_MINLEN) {
		printf("\nRunt packet, not parsing:\n");
		dump_frame(buf, len);
		return;
	}
	icmp_msg = (const struct icmp *) cur;
	if (icmp_msg->icmp_type != ICMP_ECHO) {
		printf("\n");
		return;
	}
	printf(" Echo Request\n");

	// Construct reply
	memcpy(rep, buf, len);
	cur = rep;
	rep_eth_h = (struct ether_header*) cur;
	memcpy(rep_eth_h->ether_shost, eth_dhost, ETH_ALEN);
	memcpy(rep_eth_h->ether_dhost, eth_shost, ETH_ALEN);
	cur += ETHER_HDR_LEN;
	rep_ip_hdr = (struct ip*) cur;
	rep_ip_hdr->ip_src.s_addr = ip_dst->s_addr;
	rep_ip_hdr->ip_dst.s_addr = ip_src->s_addr;
	cur += ip_hdr->ip_hl << 2;
	rep_icmp_msg = (struct icmp *) cur;
	rep_icmp_msg->icmp_type = ICMP_ECHOREPLY;
	cksum = ntohs(icmp_msg->icmp_cksum) + 0x0800;
	if (cksum > 0xffff) {
		cksum -= 0xffff;
	}
	rep_icmp_msg->icmp_cksum = htons((uint16_t) cksum);

	len_w = write(rep_fd, rep, len);
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
	uint8_t buf[1600];
	char dev[IFNAMSIZ];
	int fd1,fdm;
	fd_set fds;
	ssize_t len;

	if (argc < 2) {
		printf("Usage\n");
		exit(1);
	}

	strncpy(dev, argv[1], IFNAMSIZ);
	dev[IFNAMSIZ-1] = '\0';
	//dev[0] = '\0';
	if ((fd1 = tun_alloc(dev)) < 0) {
		printf("tun_alloc err");
		exit(1);
	}
	printf("%s\n",dev);
	fdm = fd1 + 1;
	//ioctl(fd1, TUNSETNOCSUM, 1);

	if (!inet_aton("10.0.0.2", &my_ip_addr)) {
		printf("my_ip_addr error");
		exit(1);
	}

	while (1) {
		FD_ZERO(&fds);
		FD_SET(fd1, &fds);

		select(fdm, &fds, NULL, NULL, NULL);

		if (FD_ISSET(fd1, &fds)) {
			len = read(fd1,buf,sizeof(buf));
			parse_frame(buf, len, fd1);
		}
	}
}
