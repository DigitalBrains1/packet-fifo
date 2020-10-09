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
{-# LANGUAGE MonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.ICMPEcho.FakeReply where

import Clash.Prelude
import qualified Clash.Explicit.Prelude as CEP
import Clash.Explicit.Testbench

import ICMPEcho
import Test.ICMPEcho.TestPackets
import Toolbox.Test

traceFakeReply
    :: HiddenClockResetEnable dom
    => Signal dom (Maybe (Unsigned 11))
       -- ^ Send reply
    -> Signal dom (Unsigned 8)
       -- ^ RAM read data
    -> ( Signal dom Bool
         -- ^ RAM busy
       , Signal dom (Unsigned 11)
         -- ^ RAM read address
       , Signal dom (Maybe (Bool, Unsigned 8))
         -- ^ Stream-out
       )

traceFakeReply sendReply readData = (ramBusy', readAddr', sOut')
    where
        (s', o) =   extractState fakeReply' fakeReplyIS s''
                  $ bundle (sendReply', readData)
        (cnt, total, prevB) = unbundle s'
        s'' = bundle (cnt', total', prevB')
        (ramBusy, readAddr, sOut) = unbundle o

        cnt' = traceSignal1 "cnt" cnt
        total' = traceSignal1 "total" total
        prevB' = traceSignal1 "prevB" prevB
        ramBusy' = traceSignal1 "ramBusy" ramBusy
        readAddr' = traceSignal1 "readAddr" readAddr
        sOut' = traceSignal1 "sOut" sOut
        sendReply' = traceSignal1 "sendReply" sendReply

topEntity
    :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System (Maybe (Bool, Unsigned 8))
    -> Signal System Bool
    -> ( Signal System Bool
       , Signal System (Maybe (Bool, Unsigned 8))
       )
{-# NOINLINE topEntity #-}
topEntity clk rst en sIn sOutReady = (sInReady, sOut)
    where
        (ramWrite, sInReady, sendReply)
            = withClockResetEnable clk rst en scanEcho sIn ramBusy
        (ramBusy, readAddr, sOut)
            = withClock clk $ withReset rst $ withEnable sOutEn
              traceFakeReply sendReply readData'

        readData = CEP.asyncRamPow2 clk clk en readAddr' ramWrite

        readData' = CEP.register clk rst sOutEn 0 readData
        readAddr' = CEP.register clk rst sOutEn 0 readAddr
        sOutEn = CEP.enable en sOutReady

testBench :: Signal System Bool
testBench = done
    where
        sIn = backPresStimuliGenerator clk rst (packetVecToStreamVec pktI)
            sInReady
        (sInReady, sOut) = topEntity clk rst en sIn sOutReady'
        (sOutReady, sOut')
            = stallStream clk rst en
                $(listToVecTH
                    ([ (1,1), (1,2), (1,3), (1,4), (1,5)
                     , (2,1), (2,2), (2,3), (2,4), (2,5)
                     , (3,1), (3,2), (3,3), (3,4), (3,5)
                     , (4,1), (4,2), (4,3), (4,4), (4,5)
                     , (5,1), (5,2), (5,3), (5,4), (5,5)
                     ] :: [(Int,Int)]))
                sOut
        sOutReady' = traceSignal1 "sOutReady" sOutReady
        expectedOutput
            = maybeOutputVerifier' clk rst (packetVecToStreamVec pktO)
        done = withClockResetEnable clk rst en $ expectedOutput sOut'
        clk = tbSystemClockGen (not <$> done)
        rst = systemResetGen
        en = tbEnableGen
        pktI = $(listToVecTH echoReqPacket)
        pktO = $(listToVecTH echoReplPacket)

packetVecToStreamVec pkt =    map (\e -> (True, e)) (init pkt)
                           :< (False, last pkt)

makeVCD
    = writeVCD' "fakereply.vcd"
        testBench
        [ "cnt"
        , "total"
        , "prevB"
        , "ramBusy"
        , "readAddr"
        , "sOut"
        , "sOutReady"
        , "sendReply"
        ]
