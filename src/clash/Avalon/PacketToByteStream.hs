module Avalon.PacketToByteStream where

import Clash.Prelude

import Avalon.Packet

packetToByteStream
    :: HiddenClockResetEnable dom
    => ( Signal dom Bool
       -- ^ Stream-in valid
       , Signal dom (Unsigned 32)
       -- ^ Stream-in data
       , Signal dom (Unsigned 32)
       -- ^ Stream-in other info
       )
    -> Signal dom Bool
       -- ^ Stream-out ready
    -> ( Signal dom Bool
       -- ^ Stream-in ready)
       , Signal dom (Maybe (Bool, Unsigned 8))
       -- ^ Stream-out
       )

packetToByteStream pIn sOutReady
    = o
    where
        o = mealyB packetToByteStream' ( undefined
                                       , undefined
                                       , undefined
                                       , 7 :: Unsigned 3)
                   (bundle pIn, sOutReady)


packetToByteStream' (dataReg, eop, empty, state) (pIn, sOutReady)
    = ((dataReg', eop', empty', state'), (pInReady, sOut))
    where
        (pInValid, pInData, pInOther) = pIn

        dataReg' | state == 7 = bitCoerce pInData :: Vec 4 (Unsigned 8)
                 | otherwise  = dataReg

        (eop', empty') | state == 7 = ( testBit pInOther fifoInfoEOP
                                      , truncateB $ fromFifoInfoEmpty pInOther
                                        :: Unsigned 2
                                      )
                       | otherwise  = (eop, empty)

        pInReady = state == 7
        sOut | state == 7 = Nothing
             | otherwise  = Just (more, reverse dataReg !! state)

        more = not $ eop && state == extend empty
        state'
            | pInValid && state == 7                    = 3
            | state == 7                                = 7
            | sOutReady && not more                     = 7
            | sOutReady                                 = state-1
            | otherwise                                 = state
