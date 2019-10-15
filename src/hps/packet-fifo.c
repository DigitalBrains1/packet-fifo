#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include "hwlib.h"
#include "socal/socal.h"
#include "socal/hps.h"
#include "socal/alt_gpio.h"
#include "hps_0.h"

#define HW_REGS_BASE ( ALT_STM_OFST )
#define HW_REGS_SPAN ( 0x04000000 )
#define HW_REGS_MASK ( HW_REGS_SPAN - 1 )

static inline void
write_reg32(volatile void *base, size_t offset, uint32_t data) {
	*(volatile uint32_t *) (((volatile char *) base) + offset) = data;
}

static inline uint32_t
read_reg32(volatile void *base, size_t offset) {
	return *(volatile uint32_t *) (((volatile char *) base) + offset);
}

int
main() {
	void *virtual_base;
	int fd;
	int loop_count;
	int led_direction;
	int led_mask;
	volatile uint32_t *h2p_lw_led_addr;
	volatile uint32_t *h2p_sysid_addr;
	uint32_t id;

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

	h2p_lw_led_addr = (uint32_t *) (((char *) virtual_base) +
			((size_t) (ALT_LWFPGASLVS_OFST + LED_PIO_BASE) &
			 (size_t) HW_REGS_MASK));
	h2p_sysid_addr = (uint32_t *) (((char *) virtual_base) +
			((size_t) (ALT_LWFPGASLVS_OFST + SYSID_QSYS_BASE) &
			 (size_t) HW_REGS_MASK));

	id = read_reg32(h2p_sysid_addr, 0);
	printf("%#010x\n", id);

	// toggle the LEDs a bit

	loop_count = 0;
	led_mask = 0x01;
	led_direction = 0; // 0: left to right direction
	while (loop_count < 60) {
		// control led
		write_reg32(h2p_lw_led_addr, 0, led_mask);
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
