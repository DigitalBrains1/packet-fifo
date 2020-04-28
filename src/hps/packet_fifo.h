/*
 * Functions for transferring packet data over an Intel FPGA Avalon FIFO
 * Memory Core.
 *
 * (C) Copyright 2019,2020 QBayLogic B.V.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#ifndef FILE_PACKET_FIFO_H
#define FILE_PACKET_FIFO_H
#include <stdlib.h>
#include <stdint.h>

/*
 * All the information that needs to be kept to handle mmap()ed IO regions of
 * a uio device.
 */
struct
fifo_mapped_reg {
	void *map_base; /* Location of mmap()ed region, page-aligned */
	size_t size; /* Length of mmap()ed region, page-aligned */
	void *reg_base; /* Actual MMIO register address */
};

/*
 * Context for FIFO read operation.
 */
struct
rdfifo_ctx {
	struct fifo_mapped_reg out; /* "out" register */
	struct fifo_mapped_reg csr; /* "in_csr" or "out_csr" register */
	int uio_fd; /* FD for /dev/uioX */
	size_t numbytes; /* Length in bytes of current packet in buffer */
	size_t next; /* Next word index to write in buffer */
	size_t bufsize; /* Length of buffer in words (not bytes!) */
	size_t word_cnt; /* Total number of words read from FIFO */
	uint32_t buf[]; /* Incoming packet buffer */
};

enum
wrfifo_mode {
	FIFO_FREE_RUNNING, /* No backpressure, might overflow */
	FIFO_INTR, /* An interrupt indicates available room */
};
/*
 * Context for FIFO write operation.
 */
struct
wrfifo_ctx {
	/* "in" register */
	struct fifo_mapped_reg in;

	/* "in_csr" register (optional)
	 *
	 * csr.reg_base = NULL if the CSR register is unavailable.
	 */
	struct fifo_mapped_reg csr;

	/* Operating mode */
	enum wrfifo_mode mode;

	/* FIFO depth in 32-bit words (optional)
	 *
	 * depth = -1 if unknown.
	 */
	ssize_t depth;

	/* FD for /dev/uioX (optional)
	 *
	 * Available if mode = FIFO_INTR. Otherwise, uio_fd = -1.
	 */
	int uio_fd;
};

#define FIFO_GENERAL_ERROR  (-1) /* Catch-all error */
#define FIFO_NEED_MORE      (-2) /* Packet not complete yet */
#define FIFO_OVERLONG_ERROR (-3) /* Packet is larger than buffer */
#define FIFO_DEV_ERROR      (-4) /* Error while interacting with uio device */

/*
 * Find the correct uio device by comparing the full path name or the alias in
 * the devicetree.
 *
 * If `name` starts with a '/', it is taken to be a full name. Otherwise, it
 * is taken to be a devicetree alias.
 *
 * Note that devicetree aliases have names formed from alphanumerics and
 * dashes only, and that Linux only takes aliases into account that end in a
 * number (Linux does not enforce the character set, though).
 */
extern struct uio_info_t *
fifo_uio_by_of_name(const struct uio_info_t *info, const char *of_name);

/*
 * Initialise a read-side FIFO for use.
 *
 * A context is allocated by this function and freed by close_rdfifo().
 *
 * @param ctx[out] Passes back to the caller a pointer to the allocated
 *                 context.
 * @param info The uio device for this read-side FIFO.
 * @param maxpkt The maximum length in bytes of received packets (used to
 *               allocate a large enough buffer).
 *
 * @returns 0 on success or one of the defined error codes.
 */
extern int
init_rdfifo(struct rdfifo_ctx **ctx, const struct uio_info_t *info,
		size_t maxpkt);

/*
 * Close a read-side FIFO and release all resources.
 */
extern void
close_rdfifo(struct rdfifo_ctx *ctx);

/*
 * Initialise a write-side FIFO for use.
 *
 * A context is allocated by this function and freed by close_rdfifo().
 *
 * If possible, the FIFO will have ctx->mode = FIFO_INTR, and backpressure is
 * used to prevent overflows. If the FIFO is missing the needed support,
 * ctx->mode = FIFO_FREE_RUNNING and FIFO overflows will lead to data loss and
 * potential packet corruption (two partial packets being joined into one).
 *
 * @param ctx[out] Passes back to the caller a pointer to the allocated
 *                 context.
 * @param info The uio device for this write-side FIFO.
 *
 * @returns 0 on success or one of the defined error codes.
 */
extern int
init_wrfifo(struct wrfifo_ctx **ctx, const struct uio_info_t *info);

/*
 * Close a write-side FIFO and release all resources.
 */
extern void
close_wrfifo(struct wrfifo_ctx *ctx);

/*
 * Set the threshold at which to interrupt.
 *
 * If the fill level of the FIFO drops to this many 32-bit words, an interrupt
 * is generated to indicate new data can be written to the FIFO.
 *
 * The hardware will limit the range of the value that is actually used to
 * between 1 and 1 less than the maximum fill level of the FIFO, inclusive.
 *
 * @returns The actual threshold that the hardware is using, or one of the
 * 	    defined error codes.
 */
ssize_t
set_wrfifo_thresh(const struct wrfifo_ctx *ctx, size_t thresh);

/*
 * Read from a FIFO.
 *
 * If the return value indicates a succesful read, the packet can be found in
 * the context. `ctx->numbytes` indicates the length of the packet in bytes,
 * `ctx->buf` holds the data. Casting `buf` to `uint8_t[numbytes]` is
 * sufficient.
 *
 * Keep calling `fifo_read()` until it returns `FIFO_NEED_MORE`. When it
 * returns FIFO_NEED_MORE, you can `select()` for reading on `ctx->uio_fd`. If
 * `select()` indicates readiness, do `read(ctx->uio_fd, &dummy, 4)` and then
 * invoke `fifo_read()` to read the data in the FIFO.
 *
 * The 4-byte read from `uio_fd` returns an `int32_t` counter counting the
 * number of occurred interrupts. It can be discarded in this application, but
 * needs to be read so `select()` will once again block on the next
 * invocation.
 *
 * Don't `select()` if `fifo_read()` did not return `FIFO_NEED_MORE`!
 * Furthermore, it cannot be assumed that there will be data to read when an
 * interrupt occurred; safety is only in the other direction (an interrupt
 * will definitely occur if there is data to read after `FIFO_NEED_MORE`).
 * Calling `fifo_read()` when there is no data to be read is not a problem; it
 * will just return `FIFO_NEED_MORE`.
 *
 * @param ctx The read context for the FIFO.
 *
 * @returns 0 if a whole packet has been read, `FIFO_NEED_MORE` if the
 *          end-of-packet marker has not yet been seen, or another defined
 *          error code.
 */
extern int
fifo_read(struct rdfifo_ctx *ctx);

/*
 * Write to a FIFO.
 *
 * For a FIFO with ctx->mode == FIFO_FREE_RUNNING, FIFO overflows are not
 * detected and this function always writes `len` bytes of data. This might
 * cause data loss if the FIFO is not emptied fast enough.
 *
 * A FIFO with ctx->mode == FIFO_INTR will not overflow. If the return value
 * indicates not all bytes have been written, the FIFO was full. An interrupt
 * will be generated once the free room in the FIFO drops below the configured
 * threshold (which is set to a sane value by `init_wrfifo()` and can be
 * modified using `set_wrfifo_thresh()`).
 *
 * `select()` for reading on `ctx->uio_fd` to wait for the interrupt. When
 * `select()` indicates readiness, do `read(ctx->uio_fd, &dummy, 4)`. Now
 * `fifo_write()` can be called again to write more data to the FIFO.
 *
 * The 4-byte read from `uio_fd` returns an `int32_t` counter counting the
 * number of occurred interrupts. It can be discarded in this application, but
 * needs to be read so `select()` will once again block on the next
 * invocation.
 *
 * Don't `select()` if `fifo_write()` did not indicate a short write!
 * Furthermore, it cannot be assumed that there will be space to write when an
 * interrupt occurred; safety is only in the other direction (an interrupt
 * will definitely occur if there is space to write new data). Calling
 * `fifo_write()` when there is no space to write is not a problem; it will
 * just return 0, indicating no data has been written.
 *
 * There is one unintended mode of operation. If the FIFO does not support
 * FIFO_INTR, but it does have backpressure enabled in Intel Platform Designer
 * (QSys), the whole processor will stall until the data can be written to the
 * FIFO. So there are no overruns, but only very short stalls are likely not
 * to break the overall functioning of your HPS system.
 *
 * @param ctx The write context for the FIFO.
 * @param buf The data to be written.
 * @param len The number of bytes to write.
 * @returns The actual number of bytes written to the FIFO.
 */
extern size_t
fifo_write(const struct wrfifo_ctx *ctx, const void *buf, size_t len);

#endif /* ndef FILE_PACKET_FIFO_H */
