module Test.ICMPEcho.ScanEcho where

import Clash.Prelude
import Clash.Explicit.Testbench

import ICMPEcho
import Test.ICMPEcho.TestPackets
import Toolbox.Test

packetStimuli clk rst pkt
    = stimuliGenerator clk rst (map packetStimuli' (zip indicesI pkt))
    where
        packetStimuli' (ind, v) = Just (ind /= maxBound, v)

scanEchoWrap sIn = o
    where
        (_, _, o) = scanEcho sIn (pure False)

expectedScanO pkt isEcho = expectedScanO' pkt lastE ++ replicate d16 Nothing
    where
        lastE | isEcho    = Just (fromIntegral $ length pkt - 1
                                  :: Unsigned 11)
              | otherwise = Nothing

expectedScanO'
    :: KnownNat (n+1)
    => Vec (n+1) a
    -> Maybe b
    -> Vec (n+1) (Maybe b)
expectedScanO' pkt lastE = repeat Nothing :< lastE

testBench pkt isEcho = done
    where
        testInput = packetStimuli clk rst pkt
        expectedOutput
            = outputVerifier' clk rst $ expectedScanO pkt isEcho
        done
            = exposeClockResetEnable
                (expectedOutput (scanEchoWrap $ testInput))
                clk rst en
        clk = tbSystemClockGen (not <$> done)
        rst = systemResetGen
        en = tbEnableGen

main = runTBs ([ testBench $(listToVecTH echoReqPacket) True
               , testBench $(listToVecTH tcpSynPacket)  False
               ])
