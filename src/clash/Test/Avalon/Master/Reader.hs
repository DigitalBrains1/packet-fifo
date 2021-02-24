{-
 - A design that tests the read functionality of avalonMaster
 -
 - It continually reads the "fill_level FIFO Status Register" and displays its
 - value in binary on the LEDs of the DE10 board. By writing to the FIFO from
 - the HPS, it can be verified that reading the fill_level register works as
 - the value on the LEDs increases.
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
module Test.Avalon.Master.Reader where

import Clash.Prelude

import Avalon.Master
import Test.Avalon.Common

avalonMasterReader :: PacketFifoTopEntity
avalonMasterReader = packetFifoTopEntity avalonMasterReader'
{-# ANN avalonMasterReader packetFifoSynthesize #-}
{-# NOINLINE avalonMasterReader #-}

avalonMasterReader' :: PacketFifoTopEntityImpl
avalonMasterReader' _ f2hIn h2fIn = (leds , f2hOut , h2fOut)
    where
        (f2hOut, f2hOpReady, f2hRes)
            = avalonMaster
                f2hIn f2hOp f2hResReady

        f2hResReady = pure True
        f2hOp = ( pure False, pure undefined, pure ()
                , pure (undefined :: Unsigned 3), pure undefined)

        (h2fOut, h2fOpReady, h2fRes)
            = avalonMaster
                h2fIn h2fOp h2fResReady

        (leds, h2fResReady, h2fOp)
            = ledFillLevel h2fOpReady h2fRes


        (f2hOpValid, f2hOpAddr, f2hOpData)
            = (pure False, pure undefined, pure undefined)
              :: ( Signal System Bool, Signal System (Unsigned 3)
                 , Signal System (Unsigned 32))

ledFillLevel
    :: KnownDomain dom
    => "opReady" ::: Signal dom Bool
    -> "res" ::: AvalonMasterRes dom () 32
    -> ( "leds" ::: Signal dom (Unsigned 10)
       , "resReady" ::: Signal dom Bool
       -- ^ Result ready
       , "op" ::: AvalonMasterOp dom () 6 32
       )
ledFillLevel opReady res = (leds, resReady, op)
    where
        op = (pure True, pure AvalonRead, pure (), pure 0, pure undefined)
        resReady = pure True
        (_, _, resData) = res
        leds = truncateB <$> resData
