{-
 - Generate a VCD file to verify waveforms are as expected.
 -
 - No automated testing, just manual inspection of the waveform.
 -
 - The `mockAvalonSlave` outputs a fixed data pattern. Manual inspection of
 - the waveform reveals whether `packetToByteStream` functions correctly.
 - Tuning the first argument of `mockAvalonSlave` below tests for various
 - delays.
 -
 - Note that a GTKWave save file `avp2bs.gtkw` is included for setting up the
 - wave display.
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
module Test.Avalon.PacketToByteStream where

import Clash.Prelude

import Avalon.PacketToByteStream
import Avalon.PacketReader
import Avalon.Master
import Test.Avalon.Master.MockSlave
import Toolbox.Test

mockTopEntity
    = withClockResetEnable clockGen resetGen enableGen mockTopEntity'

mockTopEntity'
    :: SystemClockResetEnable
    => Signal System (Maybe (Bool, Unsigned 8))

mockTopEntity'
    = traceSignal1 "stream" stream
    where
        (packetValid, packetData, packetOther) = packet
        packetValid' = traceSignal1 "packetValid" packetValid
        packetData' = traceSignal1 "packetData" packetData
        packetOther' = traceSignal1 "packetOther" packetOther
        packet' = (packetValid', packetData', packetOther')

        (h2fOut, h2fOpReady, h2fRes)
            = avalonMaster
                h2fIn h2fOp h2fResReady

        (h2fResReady, packet, h2fOp)
            = packetReader h2fRes (pure True) h2fOpReady

        (packetReady, stream)
            = packetToByteStream packet' streamReady

        streamReady = pure True
        h2fIn
            = mockAvalonSlave d3 h2fOut

makeVCD
    = writeVCD' "avp2bs.vcd"
        mockTopEntity
        [ "stream"
        , "packetData"
        , "packetOther"
        ]

main = makeVCD
