/*
 * Observe FIFOs in action to clarify or verify the Intel documentation.
 *
 * Either the documentation was somewhat ambiguous, or in one case
 * (tc_evint_level()) just plain wrong (unless I really misinterpreted it).
 * These tests were used to determine how it actually works.
 *
 * (C) Copyright 2019,2020 QBayLogic B.V.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * ALTERNATIVELY, this product may be distributed under the terms of the GNU
 * General Public License, either version 2 of the License, or (at your
 * option) any later version, in which case the provisions of the GPL are
 * required INSTEAD OF the above restrictions.  (This clause is necessary due
 * to a potential bad interaction between the GPL and the restrictions
 * contained in a BSD-style copyright.)
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
#include "vfy_spec.h"

#include <arpa/inet.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#include "avalon_fifo.h"
#include "mmio.h"
#include "packet_fifo.h"

static int
wait_interrupt(const struct rdfifo_ctx *f2h_ctx, struct timeval *timeout)
{
	int fdmax = f2h_ctx->uio_fd + 1;
	fd_set fds;
	uint32_t tmp;

	FD_ZERO(&fds);
	FD_SET(f2h_ctx->uio_fd, &fds);
	select(fdmax, &fds, NULL, NULL, timeout);
	if (FD_ISSET(f2h_ctx->uio_fd, &fds)) {
		read(f2h_ctx->uio_fd, &tmp, 4);
		return 1;
	}
	return 0;
}

static void
vfy_flag_set(const char *name, const void *base, size_t offset,
		uint32_t flag, int *fail)
{
	printf("%s flag: ", name);
	if (mmio_read32(base, offset) & flag) {
		printf("set\n");
	} else {
		printf("UNSET\n");
		*fail = 1;
	}
}

static void
vfy_flag_unset(const char *name, const void *base, size_t offset,
		uint32_t flag, int *fail)
{
	printf("%s flag: ", name);
	if (mmio_read32(base, offset) & flag) {
		printf("SET\n");
		*fail = 1;
	} else {
		printf("unset\n");
	}
}

static void
vfy_no_intr(const struct rdfifo_ctx *f2h_ctx, int *fail)
{
	struct timeval small_time = { 0, 50000 };

	printf("No more ints pending: ");
	if (wait_interrupt(f2h_ctx, &small_time)) {
		printf("PENDING\n");
		*fail = 1;
	} else {
		printf("none pending\n");
	}
}

static void
vfy_intr(const char *name, const struct rdfifo_ctx *f2h_ctx, int *fail)
{
	struct timeval small_time = { 0, 50000 };

	printf("Interrupt on %s: ", name);
	if (wait_interrupt(f2h_ctx, &small_time)) {
		printf("yes\n");
	} else {
		printf("NO\n");
		*fail = 1;
	}
}

static uint32_t
vfy_data(const char *name, const void *base, size_t offset,
		uint32_t expect, int *fail)
{
	uint32_t ret;

	ret = mmio_read32(base, offset);
	printf("%s: %d", name, ret);
	if (ret == expect) {
		putchar('\n');
	} else {
		printf(" - ERROR\n");
		*fail = 1;
	}
	return ret;
}

static void
init_clean(const struct rdfifo_ctx *f2h_ctx, int *fail)
{
	const void *f2h_base = f2h_ctx->out.reg_base;
	const void *f2h_csr_base = f2h_ctx->csr.reg_base;
	struct timeval small_time = { 0, 50000 };

	mmio_write32(f2h_csr_base, FIFO_IENABLE_REG, 0);
	mmio_write32(f2h_csr_base, FIFO_ALMOSTFULL_REG, 1);
	/* Flush FIFO */
	for (uint32_t i = mmio_read32(f2h_csr_base, FIFO_LEVEL_REG);
			i > 0; i--)
		mmio_read32(f2h_base, FIFO_DATA_REG);
	if (wait_interrupt(f2h_ctx, &small_time))
		printf("Note: clearing pending interrupt.\n");
	mmio_write32(f2h_csr_base, FIFO_EVENT_REG, FIFO_EVENT_ALL);
	vfy_flag_unset("AF status", f2h_csr_base, FIFO_STATUS_REG,
			FIFO_STATUS_AF, fail);
	vfy_flag_unset("AF event", f2h_csr_base, FIFO_EVENT_REG,
			FIFO_EVENT_AF, fail);
}

static void
report_result(const char *name, int fail)
{
	if (fail)
		printf("--> Testcase %s: FAILED!\n", name);
	else
		printf("--> Testcase %s: succeeded.\n", name);
}

static void
leave_clean(const struct rdfifo_ctx *f2h_ctx)
{
	const void *f2h_base = f2h_ctx->out.reg_base;
	const void *f2h_csr_base = f2h_ctx->csr.reg_base;


	mmio_write32(f2h_csr_base, FIFO_ALMOSTFULL_REG, 1);
	/* Flush FIFO */
	for (uint32_t i = mmio_read32(f2h_csr_base, FIFO_LEVEL_REG);
			i > 0; i--)
		mmio_read32(f2h_base, FIFO_DATA_REG);
	mmio_write32(f2h_csr_base, FIFO_EVENT_REG, FIFO_EVENT_ALL);
	mmio_write32(f2h_csr_base, FIFO_IENABLE_REG, 0);
}

/*
 * Testcase: Interrupt on event level-sensitive
 *
 * According to the datasheet:
 * 	When a bit in the `event` register transitions from a zero to a one,
 * 	and the corresponding bit in the `interruptenable` register is set,
 * 	the master is interrupted.
 *
 * This seems unlikely and undesirable. Edge sensitivity would probably
 * introduce race conditions.
 *
 * One would expect that the master is interrupted when the event register bit
 * is high level, not when it has an edge. Set up a test to verify this
 * hypothesis.
 *
 * This test verifies that a maintained high-level flag will trigger another
 * interrupt as soon as the enable flag is asserted.
 */
int
tc_evint_level(struct rdfifo_ctx *f2h_ctx,
		const struct wrfifo_ctx *h2f_ctx)
{
	int fail = 0;
	const void *f2h_csr_base = f2h_ctx->csr.reg_base;
	const void *f2h_base = f2h_ctx->out.reg_base;
	const uint32_t data = 0x12345678;

	printf("Running testcase: interrupt level-sensitive.\n");
	init_clean(f2h_ctx, &fail);
	mmio_write32(f2h_csr_base, FIFO_IENABLE_REG, FIFO_IENABLE_AF);
	vfy_no_intr(f2h_ctx, &fail);
	fifo_write(h2f_ctx, &data, 4);
	vfy_intr("single packet", f2h_ctx, &fail);
	vfy_no_intr(f2h_ctx, &fail);
	vfy_flag_set("AF event", f2h_csr_base, FIFO_EVENT_REG,
			FIFO_EVENT_AF, &fail);
	mmio_write32(f2h_csr_base, FIFO_IENABLE_REG, FIFO_IENABLE_AF);
	vfy_intr("high level AF", f2h_ctx, &fail);
	mmio_read32(f2h_base, FIFO_DATA_REG);
	printf("Packet read.\n");
	vfy_flag_unset("AF status", f2h_csr_base, FIFO_STATUS_REG,
			FIFO_STATUS_AF, &fail);
	vfy_flag_set("AF event", f2h_csr_base, FIFO_EVENT_REG,
			FIFO_EVENT_AF, &fail);
	mmio_write32(f2h_csr_base, FIFO_EVENT_REG, FIFO_EVENT_AF);
	printf("Event cleared.\n");
	vfy_flag_unset("AF event", f2h_csr_base, FIFO_EVENT_REG,
			FIFO_EVENT_AF, &fail);
	vfy_no_intr(f2h_ctx, &fail);
	leave_clean(f2h_ctx);
	report_result("interrupt level-sensitive", fail);
	return fail;
}

/*
 * Testcase: race interrupt enable
 *
 * See tc_evint_level() for background.
 *
 * Verify that if data arrives after we have reached fill level 0 but before
 * we have re-enabled the interrupt, we do get that interrupt afterwards.
 */
int
tc_race_int_en(struct rdfifo_ctx *f2h_ctx,
		const struct wrfifo_ctx *h2f_ctx)
{
	int fail = 0;
	const void *f2h_csr_base = f2h_ctx->csr.reg_base;
	const void *f2h_base = f2h_ctx->out.reg_base;
	const uint32_t data = 0x12345678;

	printf("Running testcase: race interrupt enable.\n");
	init_clean(f2h_ctx, &fail);
	mmio_write32(f2h_csr_base, FIFO_IENABLE_REG, FIFO_IENABLE_AF);
	vfy_no_intr(f2h_ctx, &fail);
	for (int i = 0; i < 4; i++)
		fifo_write(h2f_ctx, &data, 4);
	vfy_intr("data", f2h_ctx, &fail);
	vfy_no_intr(f2h_ctx, &fail);
	vfy_flag_set("AF event", f2h_csr_base, FIFO_EVENT_REG,
			FIFO_EVENT_AF, &fail);
	vfy_data("FIFO level", f2h_csr_base, FIFO_LEVEL_REG, 4, &fail);
	for (int i = 0; i < 4; i++)
		mmio_read32(f2h_base, FIFO_DATA_REG);
	mmio_write32(f2h_csr_base, FIFO_EVENT_REG, FIFO_EVENT_AF);
	printf("Data read and event cleared.\n");
	vfy_flag_unset("AF event", f2h_csr_base, FIFO_EVENT_REG,
			FIFO_EVENT_AF, &fail);
	vfy_data("FIFO level", f2h_csr_base, FIFO_LEVEL_REG, 0, &fail);
	fifo_write(h2f_ctx, &data, 4);
	printf("Racing data written.\n");
	vfy_no_intr(f2h_ctx, &fail);
	mmio_write32(f2h_csr_base, FIFO_IENABLE_REG, FIFO_IENABLE_AF);
	printf("Interrupts re-enabled.\n");
	vfy_intr("race", f2h_ctx, &fail);
	leave_clean(f2h_ctx);
	report_result("race interrupt enable", fail);
	return fail;
}

/*
 * Testcase: event flag edge-sensitive
 *
 * Is the event bit set when the status bit has an edge or when the status bit
 * is high level? Turns out, it's edge-sensitive indeed.
 */
int
tc_evflag_edge(struct rdfifo_ctx *f2h_ctx,
		const struct wrfifo_ctx *h2f_ctx)
{
	int fail = 0;
	const void *f2h_csr_base = f2h_ctx->csr.reg_base;
	const void *f2h_base = f2h_ctx->out.reg_base;
	const uint32_t data = 0x12345678;

	printf("Running testcase: event flag edge-sensitive.\n");
	init_clean(f2h_ctx, &fail);
	fifo_write(h2f_ctx, &data, 4);
	printf("Data written.\n");
	vfy_flag_set("AF status", f2h_csr_base, FIFO_STATUS_REG,
			FIFO_STATUS_AF, &fail);
	vfy_flag_set("AF event", f2h_csr_base, FIFO_EVENT_REG,
			FIFO_EVENT_AF, &fail);
	mmio_write32(f2h_csr_base, FIFO_EVENT_REG, FIFO_EVENT_AF);
	printf("Event cleared.\n");
	vfy_flag_set("AF status", f2h_csr_base, FIFO_STATUS_REG,
			FIFO_STATUS_AF, &fail);
	vfy_flag_unset("AF event", f2h_csr_base, FIFO_EVENT_REG,
			FIFO_EVENT_AF, &fail);
	fifo_write(h2f_ctx, &data, 4);
	printf("More data written.\n");
	vfy_flag_set("AF status", f2h_csr_base, FIFO_STATUS_REG,
			FIFO_STATUS_AF, &fail);
	vfy_flag_unset("AF event", f2h_csr_base, FIFO_EVENT_REG,
			FIFO_EVENT_AF, &fail);
	mmio_write32(f2h_csr_base, FIFO_EVENT_REG, FIFO_EVENT_AF);
	printf("Event cleared.\n");
	vfy_flag_set("AF status", f2h_csr_base, FIFO_STATUS_REG,
			FIFO_STATUS_AF, &fail);
	vfy_flag_unset("AF event", f2h_csr_base, FIFO_EVENT_REG,
			FIFO_EVENT_AF, &fail);
	vfy_data("FIFO level", f2h_csr_base, FIFO_LEVEL_REG, 2, &fail);
	mmio_read32(f2h_base, FIFO_DATA_REG);
	mmio_read32(f2h_base, FIFO_DATA_REG);
	printf("All data read.\n");
	vfy_data("FIFO level", f2h_csr_base, FIFO_LEVEL_REG, 0, &fail);
	vfy_flag_unset("AF status", f2h_csr_base, FIFO_STATUS_REG,
			FIFO_STATUS_AF, &fail);
	vfy_flag_unset("AF event", f2h_csr_base, FIFO_EVENT_REG,
			FIFO_EVENT_AF, &fail);
	mmio_write32(f2h_csr_base, FIFO_IENABLE_REG, FIFO_IENABLE_AF);
	printf("Re-enabled interrupt.\n");
	vfy_no_intr(f2h_ctx, &fail);
	leave_clean(f2h_ctx);
	report_result("event flag edge-sensitive", fail);
	return fail;
}
