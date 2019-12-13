module Test.Avalon.PacketToByteStream where

import Clash.Prelude
import qualified Data.Text.IO as TIO
import Avalon.PacketToByteStream
import Avalon.PacketReader
import Avalon.Master
import Test.Avalon.Master.MockSlave

-- traceSignal1F = flip const
traceSignal1F = traceSignal1

mockTopEntity
    = withClockResetEnable clockGen resetGen enableGen mockTopEntity'

mockTopEntity'
    :: SystemClockResetEnable
    => Signal System (Maybe (Bool, Unsigned 8))

mockTopEntity'
    = traceSignal1F "stream" stream
    where
        packet = (packetValid, packetData, packetOther)
        packetValid = traceSignal1F "packetValid" packetValid'
        packetData = traceSignal1F "packetData" packetData'
        packetOther = traceSignal1F "packetOther" packetOther'
        (packetValid', packetData', packetOther') = packet'

        (h2fOut, h2fOpReady, h2fRes)
            = avalonMaster
                h2fIn h2fResReady h2fOp

        (h2fResReady, packet', h2fOp)
            = packetReader (pure True) h2fOpReady h2fRes

        (packetReady, stream)
            = packetToByteStream packet streamReady

        streamReady = pure True
        h2fIn
            = mockAvalonSlave h2fOut

makeVCD
    = do
        Right vcd <-
            dumpVCD
                (0,2000)
                mockTopEntity
                [ "stream"
                , "packetData"
                , "packetOther"
                ]
        TIO.writeFile "avp2bs.vcd" vcd

main = makeVCD
