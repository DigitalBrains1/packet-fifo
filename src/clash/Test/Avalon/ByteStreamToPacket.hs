{-
 - Generate a VCD file to verify waveforms are as expected.
 -
 - No automated testing, just manual inspection of the waveform.
 -
 - A ByteStream-format test stream is generated. Manual inspection should
 - reveal that `packetValid`, `packetData` and `packetOther` correspond to
 - that test stream.
 -
 - Note that a GTKWave save file `avbs2p.gtkw` is included for setting up the
 - wave display.
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
{-# LANGUAGE MonomorphismRestriction #-}
module Test.Avalon.ByteStreamToPacket where

import Clash.Prelude
import Data.Maybe

import Avalon.ByteStreamToPacket
import Toolbox.Test

streamGen
    :: HiddenClockResetEnable dom
    => Signal dom Bool
    -> Signal dom (Maybe (Bool, Unsigned 8))

streamGen sOutReady
    = moore streamT streamO (1,1) sOutReady
    where
        streamT (c, t) False             = (c  , t  )
        streamT (c, t) True  | c == t    = (1  , t+1)
                             | otherwise = (c+1, t  )

        streamO (c, t) = Just (c /= t, c)

mockTopEntity
    :: Signal System (Bool, Unsigned 32, Unsigned 32)

mockTopEntity = withClockResetEnable clockGen resetGen enableGen
                                     mockTopEntity'

mockTopEntity'
    :: HiddenClockResetEnable System
    => Signal System (Bool, Unsigned 32, Unsigned 32)

mockTopEntity' = bundle packet
    where
        packet = (packetValid, packetData, packetOther)
        packetValid = traceSignal1 "packetValid" packetValid'
        packetData = traceSignal1 "packetData" packetData'
        packetOther = traceSignal1 "packetOther" packetOther'
        (packetValid', packetData', packetOther') = packet'
        streamReady = traceSignal1 "streamReady" streamReady'
        stream = mux streamValid (Just <$> ((,) <$> more <*> sData))
                                 (pure Nothing)
        streamValid = isJust <$> stream'
        more = traceSignal1 "more" more'
        sData = traceSignal1 "sData" sData'
        (more', sData')
            = unbundle $ fromJust <$> stream'

        (streamReady', packet') = byteStreamToPacket stream packetReady

        packetReady = pure True

        stream' = streamGen streamReady

makeVCD :: IO ()
makeVCD
    = writeVCD' "avbs2p.vcd"
        mockTopEntity
        [ "packetValid"
        , "packetData"
        , "packetOther"
        , "streamReady"
        , "more"
        , "sData"
        ]

main :: IO ()
main = makeVCD
