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
