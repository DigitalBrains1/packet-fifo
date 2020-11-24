{-
 - Generate a VCD file to verify waveforms are as expected.
 -
 - No automated testing, just manual inspection of the waveform.
 -
 - The `mockAvalonSlave` outputs a fixed data pattern. Manual inspection of
 - the waveform reveals whether `packetReader` functions correctly. Tuning the
 - first argument of `mockAvalonSlave` below tests for various delays.
 - `packetReader` should work correctly with a combinatorial (zero-delay)
 - Avalon slave.
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
module Test.Avalon.PacketReader where

import Clash.Prelude

import Avalon.PacketReader
import Avalon.Master
import Test.Avalon.Master.MockSlave
import Toolbox.Test

mockTopEntity
    = withClockResetEnable clockGen resetGen enableGen mockTopEntity'

mockTopEntity'
    :: SystemClockResetEnable
    => Signal System Bool

mockTopEntity'
    = pOutData' `seqXA` pOutOther' `seqXA` pOutValid'
    where
        (h2fOut, h2fOpReady, h2fRes)
            = avalonMaster
                h2fIn' h2fOp h2fResReady

        (h2fResReady, pOut, h2fOp)
            = packetReader h2fRes (pure True) h2fOpReady

        h2fIn
            = mockAvalonSlave d3 h2fOut'

        (h2fRData, h2fAck) = h2fIn
        h2fRData' = traceSignal1 "h2fRData" h2fRData
        h2fAck' = traceSignal1 "h2fAck" h2fAck
        h2fIn' = (h2fRData', h2fAck')
        (h2fAddr, h2fWData, h2fRead, h2fWrite, h2fBE) = h2fOut
        h2fAddr' = traceSignal1 "h2fAddr" h2fAddr
        h2fWData' = traceSignal1 "h2fWData" h2fWData
        h2fRead' = traceSignal1 "h2fRead" h2fRead
        h2fWrite' = traceSignal1 "h2fWrite" h2fWrite
        h2fBE' = traceSignal1 "h2fBE" h2fBE
        h2fOut' = (h2fAddr', h2fWData', h2fRead', h2fWrite', h2fBE')
        (pOutValid, pOutData, pOutOther) = pOut
        pOutValid' = traceSignal1 "pOutValid" pOutValid
        pOutData' = traceSignal1 "pOutData" pOutData
        pOutOther' = traceSignal1 "pOutOther" pOutOther

makeVCD
    = writeVCD' "avpr.vcd"
        mockTopEntity
        [ "h2fRData"
        , "h2fAck"
        , "h2fAddr"
        , "h2fWData"
        , "h2fRead"
        , "h2fWrite"
        , "h2fBE"
        , "pOutValid"
        , "pOutData"
        , "pOutOther"
        ]

main = makeVCD
