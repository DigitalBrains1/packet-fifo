{-
 - Circuit that writes test packets to the HPS.
 -
 - It can be programmed to the FPGA or observed in a VCD file.
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
{-# LANGUAGE MonomorphismRestriction #-}
module Test.Avalon.PacketWriter where

import Clash.Prelude

import qualified Data.Text.IO as TIO
import Avalon.PacketWriter
import Avalon.Packet
import Avalon.Master
import Test.Avalon.Master.MockSlave
import Toolbox.Misc
import Toolbox.Test

{-# ANN avalonPacketWriter
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
avalonPacketWriter
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
avalonPacketWriter clk rst_n
    = exposeClockResetEnable avalonPacketWriter' clk rstS enableGen
    where
        rstS = resetSynchronizer clk (unsafeFromLowPolarity rst_n) enableGen
{-# NOINLINE avalonPacketWriter #-}

avalonPacketWriter'
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

avalonPacketWriter' _ f2hIn h2fIn
    = (pure 0, f2hOut , h2fOut)
    where
        (f2hOut, f2hOpReady, f2hRes)
            = avalonMaster f2hIn f2hOp f2hResReady

        (h2fOut, h2fOpReady, h2fRes)
            = avalonMaster h2fIn h2fOp h2fResReady

        h2fResReady = pure True
        h2fOp = ( pure False, pure undefined, pure ()
                , pure (undefined :: Unsigned 6), pure undefined)

        (pInReady, f2hOp) = packetWriter pIn f2hOpReady

        f2hResReady = pure True

        pIn = writeDummyCounter pInReady

{-
 - Write test packet data.
 -
 - 128 8-byte/2-word packets are output. The two words that make up the eight
 - bytes are incrementing from 0 to 255, split out over the 128 packets.
 -}
writeDummyCounter
    :: HiddenClockResetEnable dom
    => "pInReady" ::: Signal dom Bool
    -> "pIn" :::
           ( "pInValid" ::: Signal dom Bool
           -- ^ Stream-in valid
           , "pInData" ::: Signal dom (Unsigned 32)
           -- ^ Stream-in data
           , "pInOther" ::: Signal dom (Unsigned 32)
           -- ^ Stream-in other info
           )
writeDummyCounter pInReady
    = mealyB writeDummyCounter' 0 pInReady

writeDummyCounter'
    :: Mealy ( "s" ::: "cnt" ::: Unsigned 32)
             ( "i" ::: "pInReady" ::: Bool)
             ( "o" :::
                 "pIn" :::
                     ( "pInValid" ::: Bool
                     , "pInData" ::: Unsigned 32
                     , "pinOther" ::: Unsigned 32
                     )
             )

writeDummyCounter' cnt pInReady
    = (cnt', (pInValid, cnt, other))
    where
        maxCnt = 256
        pInValid = cnt /= maxCnt
        cnt' | cnt == maxCnt = cnt
             | pInReady      = cnt + 1
             | otherwise     = cnt
        other | lsb cnt == 0 = bit fifoInfoSOP
              | otherwise    = bit fifoInfoEOP

{-
 - Add signal tracing to avalonPacketWriter.
 -
 - In order to enable making a VCD file, traces are set on the connections to
 - the External Bus to Avalon Bridge.
 -
 - The output signal's only purpose is to force certain signals to be
 - evaluated during simulation. It is possible this is leftover code that is
 - no longer necessary.
 -}
mockTopEntity
    :: Signal System Bool

mockTopEntity = f2hRData' `seqXA` f2hAck'
    where
        (f2hRData, f2hAck) = f2hIn
        f2hRData' = traceSignal1 "f2hRData" f2hRData
        f2hAck' = traceSignal1 "f2hAck" f2hAck

        (_, f2hOut, _)
            = avalonPacketWriter
                clockGen (pure True) (pure 0b111) f2hIn
                (pure undefined, pure False)

        (f2hAddr, f2hWData, f2hRead, f2hWrite, f2hBE) = f2hOut
        f2hAddr' = traceSignal1 "f2hAddr" f2hAddr
        f2hWData' = traceSignal1 "f2hWData" f2hWData
        f2hRead' = traceSignal1 "f2hRead" f2hRead
        f2hWrite' = traceSignal1 "f2hWrite" f2hWrite
        f2hBE' = traceSignal1 "f2hBE" f2hBE
        f2hOut' = (f2hAddr', f2hWData', f2hRead', f2hWrite', f2hBE')
        f2hIn
            = withClockResetEnable
                clockGen resetGen enableGen
                mockAvalonSlave d3 f2hOut'

makeVCD :: IO ()
makeVCD
    = writeVCD' "avpw.vcd"
        mockTopEntity
        [ "f2hAddr"
        , "f2hWData"
        , "f2hRead"
        , "f2hWrite"
        , "f2hBE"
        , "f2hRData"
        , "f2hAck"
        ]
