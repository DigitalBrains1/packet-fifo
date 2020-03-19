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

main = makeVCD
