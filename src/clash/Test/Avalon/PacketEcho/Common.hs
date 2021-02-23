{-
 - A design that echoes any packets written to the FPGA back to the HPS.
 -
 - An optional transformation enables experimenting with the layout of data.
 - Actual top entities are defined in different files for specific
 - transformations.
 -
 - Packets pass through avalonMaster, packetReader and packetWriter, verifying
 - their basic functioning.
 -
 - Copyright (c) 2019-2021 QBayLogic B.V.
 - All rights reserved.
 -
 - Redistribution and use in source and binary forms, with or without
 - modification, are permitted provided that the following conditions are met:
 -
 - 1. Redistributions of source code must retain the above copyright notice,
 - this list of conditions and the following disclaimer.
 -
 - 2. Redistributions in binary form must reproduce the above copyright notice,
 - this list of conditions and the following disclaimer in the documentation
 - and/or other materials provided with the distribution.
 -
 - THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 - AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 - IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 - ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 - LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 - CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 - SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 - INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 - CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 - ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 - POSSIBILITY OF SUCH DAMAGE.
 -}
module Test.Avalon.PacketEcho.Common where

import Clash.Prelude

import Avalon.Master
import Avalon.PacketReader
import Avalon.PacketWriter

avalonPacketEchoXfm
    :: SystemClockResetEnable
    => (Unsigned 32 -> Unsigned 32)
    -> Signal System (BitVector 3)
    -- ^ fpga_debounced_buttons
    -> ( Signal System (Unsigned 32)
       -- ^ fifo_f2h_in_mm_external_interface_read_data
       , Signal System Bool
       -- ^ fifo_f2h_in_mm_external_interface_acknowledge
       )
    -> ( Signal System (Unsigned 32)
       -- ^ fifo_h2f_out_mm_external_interface_read_data
       , Signal System Bool
       -- ^ fifo_h2f_out_mm_external_interface_acknowledge
       )
    -> ( Signal System (Unsigned 10)
       -- ^ fpga_led_internal
       , ( Signal System (Unsigned 3)
         -- ^ fifo_f2h_in_mm_external_interface_address
         , Signal System (Unsigned 32)
         -- ^ fifo_f2h_in_mm_external_interface_write_data
         , Signal System Bool
         -- ^ fifo_f2h_in_mm_external_interface_read
         , Signal System Bool
         -- ^ fifo_f2h_in_mm_external_interface_write
         , Signal System (BitVector 4)
         -- ^ fifo_f2h_in_mm_external_interface_byte_enable
         )
       , ( Signal System (Unsigned 6)
         -- ^ fifo_h2f_out_mm_external_interface_address
         , Signal System (Unsigned 32)
         -- ^ fifo_h2f_out_mm_external_interface_write_data
         , Signal System Bool
         -- ^ fifo_h2f_out_mm_external_interface_read
         , Signal System Bool
         -- ^ fifo_h2f_out_mm_external_interface_write
         , Signal System (BitVector 4)
         -- ^ fifo_h2f_out_mm_external_interface_byte_enable
         )
       )

avalonPacketEchoXfm xfm _ f2hIn h2fIn
    = (pure 0, f2hOut, h2fOut)
    where
        (f2hOut, f2hOpReady, f2hRes)
            = avalonMaster
                f2hIn f2hOp f2hResReady

        (h2fOut, h2fOpReady, h2fRes)
            = avalonMaster
                h2fIn h2fOp h2fResReady

        (h2fResReady, pOut, h2fOp)
            = packetReader h2fRes packetReady h2fOpReady

        (packetReady, f2hOp) = packetWriter pIn f2hOpReady
        f2hResReady = pure True

        (pOutValid, pOutData, pOutOther) = pOut
        pInData = xfm <$> pOutData
        pIn = (pOutValid, pInData, pOutOther)
