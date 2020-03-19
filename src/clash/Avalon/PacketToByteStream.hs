module Avalon.PacketToByteStream where

import Clash.Prelude

import Avalon.Packet
import Toolbox.Misc

{-
 - Convert Avalon packet format to stream of bytes.
 -
 - `pIn` is in the format as produced by Avalon.PacketReader (which itself is
 - very similar to the Avalon-ST packet interface). This is converted to a
 - stream of byte values with a marker indicating end-of-packet.
 -
 - Output is transferred whenever `isJust sOut` and `sOutReady` is asserted.
 -
 - `more` indicates there are more bytes in the packet and is de-asserted for
 - the last byte in the packet. THe start of the next packet is implied to be
 - at the next byte. NOTE that the notion of data in-between, and thus not
 - part of a packet is LOST by this function. It only uses `fifoInfoEOP` as a
 - delimiter, `fifoInfoSOP` is not inspected.
 -}
packetToByteStream
    :: HiddenClockResetEnable dom
    => "pIn" :::
           ( "pInValid" ::: Signal dom Bool
           -- ^ Stream-in valid
           , "pInData" ::: Signal dom (Unsigned 32)
           -- ^ Stream-in data
           , "pInOther" ::: Signal dom (Unsigned 32)
           -- ^ Stream-in other info
           )
    -> "sOutReady" ::: Signal dom Bool
       -- ^ Stream-out ready
    -> ( "pInReady" ::: Signal dom Bool
       -- ^ Stream-in ready)
       , "sOut" :::
             Signal dom
                 (Maybe ( "more" ::: Bool
                        , "data" ::: Unsigned 8
                        )
                 )
       -- ^ Stream-out
       )

packetToByteStream pIn sOutReady
    = o
    where
        o = mealyB packetToByteStream' ( undefined
                                       , undefined
                                       , undefined
                                       , 7)
                   (bundle pIn, sOutReady)

packetToByteStream'
    :: Mealy ( "s" :::
                 ( "dataReg" ::: Vec 4 (Unsigned 8)
                 , "eop" ::: Bool
                 , "empty" ::: Unsigned 2
                 , "state" ::: Unsigned 3
                 )
             )
             ( "i" :::
                 ( "pIn" :::
                        ( "pInReady" ::: Bool
                        , "pInData" ::: Unsigned 32
                        , "pInOther" ::: Unsigned 32
                        )
                 , "sOutReady" ::: Bool
                 )
             )
             ( "o" :::
                 ( "pInReady" ::: Bool
                 , "sOut" :::
                       Maybe ( "more" ::: Bool
                             , "data" ::: Unsigned 8
                             )
                 )
             )

packetToByteStream' (dataReg, eop, empty, state) (pIn, sOutReady)
    = ((dataReg', eop', empty', state'), (pInReady, sOut))
    where
        (pInValid, pInData, pInOther) = pIn

        dataReg' | state == 7 = bitCoerce pInData
                 | otherwise  = dataReg

        (eop', empty') | state == 7 = ( testBit pInOther fifoInfoEOP
                                      , truncateB $ fromFifoInfoEmpty pInOther
                                      )
                       | otherwise  = (eop, empty)

        pInReady = state == 7
        sOut | state == 7 = Nothing
             | otherwise  = Just (more, reverse dataReg !! state)

        more = not $ eop && state == extend empty
        state' | pInValid && state == 7 = 3
               | state == 7             = 7
               | sOutReady && not more  = 7
               -- Note that this wraps from 0 to 7
               | sOutReady              = state - 1
               | otherwise              = state
