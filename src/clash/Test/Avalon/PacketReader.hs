module Test.Avalon.PacketReader where

import Clash.Prelude
import qualified Data.Text.IO as TIO
import Avalon.PacketReader
import Avalon.Master
import Test.Avalon.Master.MockSlave

{- traceSignal1 is giving some issues: it keeps complaining "Already tracing a
 - signal with the name: ..." when this is not the case. Current code randomly
 - works. At least at the moment...
 -}
-- traceSignal1F = flip const
traceSignal1F = traceSignal1

mockTopEntity
    = withClockResetEnable clockGen resetGen enableGen mockTopEntity'

mockTopEntity'
    :: SystemClockResetEnable
    => Signal System Bool

mockTopEntity'
--    = flatten <$> pOutValid <*> pOutData <*> pOutOther
    = pOutValid
    where
        h2fRData = traceSignal1F "h2fRData" h2fRData'
        h2fAck = traceSignal1F "h2fAck" h2fAck'
        (h2fRData', h2fAck') = h2fIn'
        h2fAddr = traceSignal1F "h2fAddr" h2fAddr'
        h2fWData = traceSignal1F "h2fWData" h2fWData'
        h2fRead = traceSignal1F "h2fRead" h2fRead'
        h2fWrite = traceSignal1F "h2fWrite" h2fWrite'
        h2fBE = traceSignal1F "h2fBE" h2fBE'
        pOutValid = traceSignal1F "pOutValid" pOutValid'
        pOutData = traceSignal1F "pOutData" pOutData'
        pOutOther = traceSignal1F "pOutOther" pOutOther'
        (pOutValid', pOutData', pOutOther') = pOut'
        (h2fAddr', h2fWData', h2fRead', h2fWrite', h2fBE') = h2fOut'
        h2fIn = (h2fRData, h2fAck)
        h2fOut = (h2fAddr, h2fWData, h2fRead, h2fWrite, h2fBE)

        (h2fOut', h2fOpReady, h2fRes)
            = avalonMaster
                h2fIn h2fOp h2fResReady

        (h2fResReady, pOut', h2fOp)
            = packetReader h2fRes (pure True) h2fOpReady

        h2fIn'
            = mockAvalonSlave h2fOut

        flatten a b c = reduceOr (   reduceOr a :> reduceOr b :> reduceOr c
                                  :> Nil)

makeVCD
    = do
        Right vcd <-
            dumpVCD
                (0,2000)
                mockTopEntity
                [ "h2fRData"
                , "h2fAck"
                , "h2fAddr"
                , "h2fWData"
                , "h2fRead"
                , "h2fWrite"
                , "h2fBE"
                , "pOutValid"
                --, "pOutData", "pOutOther"
                ]
        TIO.writeFile "avpr.vcd" vcd

main = makeVCD
