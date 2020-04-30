#ifndef FILE_VFY_SPEC_H
#define FILE_VFY_SPEC_H
#include "packet_fifo.h"
/*
 * Testcase: Interrupt on event level-sensitive
 */
int
tc_evint_level(struct rdfifo_ctx *f2h_ctx,
		const struct wrfifo_ctx *h2f_ctx);

/*
 * Testcase: race interrupt enable
 */
int
tc_race_int_en(struct rdfifo_ctx *f2h_ctx,
		const struct wrfifo_ctx *h2f_ctx);

/*
 * Testcase: event flag edge-sensitive
 */
int
tc_evflag_edge(struct rdfifo_ctx *f2h_ctx,
		const struct wrfifo_ctx *h2f_ctx);
#endif /* ndef FILE_VFY_SPEC_H */
