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
upload: fpgaeth test-fifo
	rsync -a --no-g --no-o $^ root@de10:packet-fifo/

fpgaeth: fpgaeth.o packet_fifo.o uio_helper.o

test-fifo: test-fifo.o packet_fifo.o uio_helper.o

%.o.lst: %.o
#	$(OBJDUMP) -rtdg $< >$@
#	$(OBJDUMP) -rtzsSgfhFpW --special-syms $< >$@
	$(OBJDUMP) -S $< >$@

%.c.i: %.c
	$(CC) $(CFLAGS) -E -o $@ $<

%.c.s: %.c
	$(CC) $(CFLAGS) -S -o $@ $<

%.dtb: %.dts
	dtc -@ -o $@ $<

# Pull in dependency info for *existing* .o files
-include $(OBJS:.o=.d)

# Compile and generate dependency info;
# more complicated dependency computation, so all prereqs listed
# will also become command-less, prereq-less targets
#   sed:    strip the target (everything before colon)
#   sed:    remove any continuation backslashes
#   fmt -1: list words one per line
#   sed:    strip leading spaces
#   sed:    add trailing colons
%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<
	$(CC) -MM $(CFLAGS) $< > $*.d
	@cp -f $*.d $*.d.tmp
	@sed -e 's/.*://' -e 's/\\$$//' < $*.d.tmp | fmt -1 | \
	  sed -e 's/^ *//' -e 's/$$/:/' >> $*.d
	@rm -f $*.d.tmp

.PHONY: clean
clean:
	rm -f fpgaeth test-fifo overlay.dtb *.o *.o.lst *.c.i *.c.s *.d
