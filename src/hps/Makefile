ALT_DEVICE_FAMILY ?= soc_cv_av
SOCEDS_ROOT ?= $(SOCEDS_DEST_ROOT)
HWLIBS_ROOT = $(SOCEDS_ROOT)/ip/altera/hps/altera_hps/hwlib
CROSS_COMPILE = arm-linux-gnueabihf-
CFLAGS = -O2 -g -std=gnu11 -Wall -Wextra -Wpedantic -D$(ALT_DEVICE_FAMILY) -I$(HWLIBS_ROOT)/include/$(ALT_DEVICE_FAMILY) -I$(HWLIBS_ROOT)/include/
LDFLAGS = -g -Wall
CC = $(CROSS_COMPILE)gcc
OBJDUMP = $(CROSS_COMPILE)objdump
ARCH= arm

.PHONY: all
all: upload

.PHONY: upload
upload: test-fifo
	rsync -a --no-g --no-o $^ root@de10:packet-fifo/

test-fifo: test-fifo.o packet-fifo.o

%.o.lst: %.o
#	$(OBJDUMP) -rtdg $< >$@
#	$(OBJDUMP) -rtzsSgfhFpW --special-syms $< >$@
	$(OBJDUMP) -S $< >$@

%.c.i: %.c
	$(CC) $(CFLAGS) -E -o $@ $<

%.c.s: %.c
	$(CC) $(CFLAGS) -S -o $@ $<

.PHONY: clean
clean:
	rm -f test-fifo *.o *.o.lst *.c.i *.c.s
