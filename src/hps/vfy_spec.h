/*
 * Observe FIFOs in action to clarify or verify the Intel documentation.
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
