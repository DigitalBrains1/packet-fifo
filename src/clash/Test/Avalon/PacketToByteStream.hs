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
