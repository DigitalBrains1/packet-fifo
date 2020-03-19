/*
 * Register definitions for the Intel FPGA Avalon FIFO Memory Core.
 *
 * Based on Intel's altera_avalon_fifo_regs.h
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
