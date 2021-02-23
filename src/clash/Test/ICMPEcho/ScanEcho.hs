{-
 - Test packet recognition logic in ICMPEcho.scanEcho.
 -
 - While it is named "testBench", this module is not for generating HDL.
 - Instead, this module's main function will run two tests in Haskell
 - simulation.
 -
 - Two packets are fed to scanEcho, an ICMP Echo Request and a TCP SYN packet.
 - It is verified that the former triggers a match by scanEcho, while the
 - latter should not trigger a match.
 -
 - Copyright (c) 2019-2021 QBayLogic B.V.
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

packetStimuli
    :: KnownNat n
    => Clock System
    -> Reset System
    -> "pkt" ::: Vec n (Unsigned 8)
    -> "sOut" ::: Signal System (Maybe ( "more" ::: Bool
                                       , "data" ::: Unsigned 8))
packetStimuli clk rst pkt
    = stimuliGenerator clk rst (map packetStimuli' (zip indicesI pkt))
    where
        packetStimuli' (ind, v) = Just (ind /= maxBound, v)

scanEchoWrap
    :: SystemClockResetEnable
    => "sIn" ::: Signal System (Maybe ( "more" ::: Bool
                                      , "data" ::: Unsigned 8))
    -> "sendReply" ::: Signal System (Maybe ("pktLen" ::: Unsigned 11))
scanEchoWrap sIn = o
    where
        (_, _, o) = scanEcho sIn (pure False)

expectedScanO
    :: forall n
     . KnownNat n
    => "pktLen" ::: SNat (n+1)
    -> "isEcho" ::: Bool
    -> "sendReplyVals" ::: Vec (n+17) (Maybe (Unsigned 11))
expectedScanO _ isEcho = (repeat Nothing :< lastE) ++ replicate d16 Nothing
    where
        lastE | isEcho    = Just (natToNum @n @(Unsigned 11))
              | otherwise = Nothing

testBench
    :: KnownNat n
    => Vec (n+1) (Unsigned 8)
    -> Bool
    -> Signal System Bool
testBench pkt isEcho = done
    where
        testInput = packetStimuli clk rst pkt
        expectedOutput
            = outputVerifier' clk rst $ expectedScanO (lengthS pkt) isEcho
        done
            = exposeClockResetEnable
                (expectedOutput (scanEchoWrap $ testInput))
                clk rst en
        clk = tbSystemClockGen (not <$> done)
        rst = systemResetGen
        en = tbEnableGen

main :: IO ()
main = runTBs ([ testBench $(listToVecTH echoReqPacket) True
               , testBench $(listToVecTH tcpSynPacket)  False
               ])
