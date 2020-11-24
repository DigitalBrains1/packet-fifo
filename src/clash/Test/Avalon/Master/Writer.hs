{-
 - A design that tests the write functionality of avalonMaster.
 -
 - It writes 256 32-bit words with an increasing number to the data register
 - of the FPGA-to-HPS FIFO. While this is not properly formatted for sending
 - packets, since it lacks the SOP and EOP start and end markers, it will
 - still be available for the HPS to read. Avalon-ST interfaces have the
 - concept of "inter-packet data" which is otherwise completely ignored in
 - this Clash implementation, but what this design produces is probably
 - inter-packet data, without any surrounding packets.
 -
 - So by reading the 256 words from the HPS side, it can be verified that
 - basic write functionality of avalonMaster works.
 -
 - Alternatively, the behavior of avalonMaster can be observed in a VCD file.
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
module Test.Avalon.Master.Writer where

import Clash.Prelude

import Avalon.Master
import Test.Avalon.Master.MockSlave
import Toolbox.Misc
import Toolbox.Test

{-# ANN avalonMasterWriter
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
avalonMasterWriter
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
avalonMasterWriter clk rst_n
    = exposeClockResetEnable avalonMasterWriter' clk rstS enableGen
    where
        rstS = resetSynchronizer clk (unsafeFromLowPolarity rst_n) enableGen
{-# NOINLINE avalonMasterWriter #-}

avalonMasterWriter'
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
avalonMasterWriter' _ f2hIn h2fIn
    = (pure 0, f2hOut , h2fOut)
    where
        (f2hOut, f2hOpReady, f2hRes)
            = avalonMaster f2hIn f2hOp (pure True)

        (h2fOut, h2fOpReady, h2fRes)
            = avalonMaster h2fIn h2fOp h2fResReady

        h2fResReady = pure True
        h2fOp = ( pure False, pure undefined, pure ()
                , pure undefined, pure undefined)

        f2hOp = writeDummyCounter f2hOpReady'
        f2hOpReady' = traceSignal1 "f2hOpReady" f2hOpReady

writeDummyCounter
    :: HiddenClockResetEnable dom
    => "opReady" ::: Signal dom Bool
    -> "op" :::
        ( "opValid" ::: Signal dom Bool
        -- ^ Operation valid
        , "opCmd" ::: Signal dom AvalonCmd
        -- ^ Read or write
        , "opTag" ::: Signal dom ()
        -- ^ Passthrough tag for request matching
        , "opAddr" ::: Signal dom (Unsigned 3)
        -- ^ Address
        , "opData" ::: Signal dom (Unsigned 32)
        -- ^ Write data
        )
writeDummyCounter opReady
    = mealyB writeDummyCounter' 0 opReady

writeDummyCounter'
    :: Mealy ("s" ::: "cnt" ::: Unsigned 32)
             ("i" ::: "opReady" ::: Bool)
             ("o" ::: "op" :::
                 ( "opValid" ::: Bool
                 -- ^ Operation valid
                 , "opCmd" ::: AvalonCmd
                 -- ^ Read or write
                 , "opTag" ::: ()
                 -- ^ Passthrough tag for request matching
                 , "opAddr" ::: Unsigned 3
                 -- ^ Address
                 , "opData" ::: Unsigned 32
                 -- ^ Write data
                 )
             )
writeDummyCounter' cnt opReady = (cnt', (opValid, AvalonWrite, (), 0, cnt))
    where
        maxCnt = 256
        opValid = cnt /= maxCnt
        cnt' | cnt == maxCnt = cnt
             | opReady       = cnt + 1
             | otherwise     = cnt

{-
 - Add signal tracing to avalonMasterWriter.
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
            = avalonMasterWriter
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
            = withClockResetEnable clockGen resetGen enableGen
                mockAvalonSlave d1 f2hOut'

makeVCD :: IO ()
makeVCD
    = writeVCD' "avmw.vcd"
        mockTopEntity
        [ "f2hAddr"
        , "f2hWData"
        , "f2hRead"
        , "f2hWrite"
        , "f2hBE"
        , "f2hRData"
        , "f2hAck"
        , "f2hOpReady"
        ]
