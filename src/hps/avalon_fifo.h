/*
 * Register definitions for the Intel FPGA Avalon FIFO Memory Core.
 *
 * Based on Intel's altera_avalon_fifo_regs.h
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
#ifndef FILE_AVALON_FIFO_H
#define FILE_AVALON_FIFO_H

/*
 * Register offsets are byte addresses.
 */

#define FIFO_DATA_REG		0
#define FIFO_OTHER_INFO_REG	4

#define FIFO_INFO_SOP		1
#define FIFO_INFO_EOP		2
#define FIFO_INFO_EMPTY_SHIFT	2
#define FIFO_INFO_EMPTY_MASK	(0x1f << FIFO_INFO_EMPTY_SHIFT)

#define FIFO_INFO_EMPTY_GET(reg) \
	(((reg) & FIFO_INFO_EMPTY_MASK) >> FIFO_INFO_EMPTY_SHIFT)
#define FIFO_INFO_EMPTY_SET(val) \
	(((val) << FIFO_INFO_EMPTY_SHIFT) & FIFO_INFO_EMPTY_MASK)

#define FIFO_LEVEL_REG                         0
#define FIFO_STATUS_REG                        4
#define FIFO_EVENT_REG                         8
#define FIFO_IENABLE_REG                       12
#define FIFO_ALMOSTFULL_REG                    16
#define FIFO_ALMOSTEMPTY_REG                   20

#define FIFO_EVENT_F    (0x01)
#define FIFO_EVENT_E    (0x02)
#define FIFO_EVENT_AF   (0x04)
#define FIFO_EVENT_AE   (0x08)
#define FIFO_EVENT_OVF  (0x10)
#define FIFO_EVENT_UDF  (0x20)
#define FIFO_EVENT_ALL  (0x3F)

#define FIFO_STATUS_F    (0x01)
#define FIFO_STATUS_E    (0x02)
#define FIFO_STATUS_AF   (0x04)
#define FIFO_STATUS_AE   (0x08)
#define FIFO_STATUS_OVF  (0x10)
#define FIFO_STATUS_UDF  (0x20)
#define FIFO_STATUS_ALL  (0x3F)

#define FIFO_IENABLE_F    (0x01)
#define FIFO_IENABLE_E    (0x02)
#define FIFO_IENABLE_AF   (0x04)
#define FIFO_IENABLE_AE   (0x08)
#define FIFO_IENABLE_OVF  (0x10)
#define FIFO_IENABLE_UDF  (0x20)
#define FIFO_IENABLE_ALL  (0x3F)

#endif /* ndef FILE_AVALON_FIFO_H */
