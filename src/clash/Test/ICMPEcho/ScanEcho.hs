{-
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
