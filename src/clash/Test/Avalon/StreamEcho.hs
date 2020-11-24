{-
 - Example application echoing all data.
 -
 - This can serve as a starting point for writing actual applications.
 -
 - In addition, it verifies basic functionality.
 -
 - Pass all packets sent to the FPGA from the HPS back to the HPS, passing
 - through avalonMaster, avalonPacketReader, packetToByteStream,
 - byteStreamToPacket and avalonPacketWriter.
 -
 - Copyright (c) 2019, 2020 QBayLogic B.V.
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
module Test.Avalon.StreamEcho where

import Clash.Prelude

import Avalon.Master
import Avalon.PacketReader
import Avalon.PacketWriter
import Avalon.PacketToByteStream
import Avalon.ByteStreamToPacket

{-# ANN avalonStreamEcho
    (Synthesize
        { t_name   = "packet_fifo"
        , t_inputs
            = [ PortName "clk"
              , PortName "rst_n"
              , PortName "fpga_debounced_buttons"
              , avalonMasterExtInputNames "fifo_f2h_in_mm_"
              , avalonMasterExtInputNames "fifo_h2f_out_mm_"
              ]
        , t_output
            = PortProduct ""
                [ PortName "fpga_led_internal"
                , avalonMasterExtOutputNames "fifo_f2h_in_mm_"
                , avalonMasterExtOutputNames "fifo_h2f_out_mm_"
                ]
        }) #-}
avalonStreamEcho
    :: Clock System
    -> "rst_n" ::: Signal System Bool
    -- ^ Active-low asynchronous reset
    -> "fpga_debounced_buttons" ::: Signal System (BitVector 3)
    -> "fifo_f2h_in" :::
           ( Signal System (Unsigned 32)
           -- ^ fifo_f2h_in_mm_external_interface_read_data
           , Signal System Bool
           -- ^ fifo_f2h_in_mm_external_interface_acknowledge
           )
    -> "fifo_f2h_out" :::
           ( Signal System (Unsigned 32)
           -- ^ fifo_h2f_out_mm_external_interface_read_data
           , Signal System Bool
           -- ^ fifo_h2f_out_mm_external_interface_acknowledge
           )
    -> ( "fpga_led_internal" ::: Signal System (Unsigned 10)
       , "fifo_f2h_in" :::
             ( Signal System (Unsigned 3)
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
       , "fifo_f2h_out" :::
             ( Signal System (Unsigned 6)
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

avalonStreamEcho clk rst_n
    = withClockResetEnable clk rstS enableGen avalonStreamEcho'
    where
        rstS = resetSynchronizer clk (unsafeFromLowPolarity rst_n) enableGen
{-# NOINLINE avalonStreamEcho #-}

avalonStreamEcho'
    :: SystemClockResetEnable
    => "fpga_debounced_buttons" ::: Signal System (BitVector 3)
    -> "fifo_f2h_in" :::
           ( Signal System (Unsigned 32)
           -- ^ fifo_f2h_in_mm_external_interface_read_data
           , Signal System Bool
           -- ^ fifo_f2h_in_mm_external_interface_acknowledge
           )
    -> "fifo_f2h_out" :::
           ( Signal System (Unsigned 32)
           -- ^ fifo_h2f_out_mm_external_interface_read_data
           , Signal System Bool
           -- ^ fifo_h2f_out_mm_external_interface_acknowledge
           )
    -> ( "fpga_led_internal" ::: Signal System (Unsigned 10)
       , "fifo_f2h_in" :::
             ( Signal System (Unsigned 3)
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
       , "fifo_f2h_out" :::
             ( Signal System (Unsigned 6)
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

avalonStreamEcho' _ f2hIn h2fIn
    = (pure 0, f2hOut, h2fOut)
    where
        (f2hOut, f2hOpReady, f2hRes)
            = avalonMaster
                f2hIn f2hOp f2hResReady

        (h2fOut, h2fOpReady, h2fRes)
            = avalonMaster
                h2fIn h2fOp h2fResReady

        (h2fResReady, h2fPacket, h2fOp)
            = packetReader h2fRes h2fPacketReady h2fOpReady

        (f2hPacketReady, f2hOp) = packetWriter f2hPacket f2hOpReady
        f2hResReady = pure True

        (h2fPacketReady, stream)
            = packetToByteStream h2fPacket streamReady

        (streamReady, f2hPacket)
            = byteStreamToPacket stream f2hPacketReady
