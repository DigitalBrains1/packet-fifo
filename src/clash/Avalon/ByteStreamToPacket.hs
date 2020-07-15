module Avalon.ByteStreamToPacket where

import Clash.Prelude
import Data.Maybe

import Avalon.Packet

byteStreamToPacket
    :: HiddenClockResetEnable dom
    => Signal dom (Maybe (Bool, Unsigned 8))
       -- ^ Stream-in
    -> Signal dom Bool
       -- ^ Packet-out ready
    -> (Signal dom Bool
        -- ^ Stream-in ready
       , ( Signal dom Bool
       -- ^ Packet-out valid
         , Signal dom (Unsigned 32)
         -- ^ Packet-out data
         , Signal dom (Unsigned 32)
         -- ^ Packet-out other info
         )
       )

byteStreamToPacket sIn pOutReady = (sInReady, unbundle pOut)
    where
        (sInReady, pOut) = mealyB byteStreamToPacket' ( repeat 0
                                                      , True
                                                      , undefined
                                                      , 0
                                                      , 3 :: Unsigned 3)
                                  (bundle sIn, pOutReady)

byteStreamToPacket' (dataReg, sop, eop, empty, state) (sIn, pOutReady)
     = ((dataReg', sop', eop', empty', state'), (sInReady, pOut))
     where
        (more, sInData) = fromJust sIn
        pOut = (pOutValid, bitCoerce $ reverse dataReg, otherReg)

        pOutValid = state == 7
        sInReady = not pOutValid
        otherReg =     (unpack $    (boolToBV sop `shiftL` fifoInfoSOP)
                                .|. (boolToBV eop `shiftL` fifoInfoEOP)
                       )
                   .|. toFifoInfoEmpty (extend empty)

        dataReg' | isJust sIn && state /= 7 = replace state sInData dataReg
                                              :: Vec 4 (Unsigned 8)
                 | otherwise                = dataReg

        sop' | state == 7 && state' /= 7 = eop
             | otherwise                 = sop

        eop' | state == 7 && state' /= 7 = False
             | isJust sIn && not more    = True
             | otherwise                 = eop

        empty' | state /= 7 && isJust sIn && not more = truncateB state
                                                        :: Unsigned 2
               | otherwise                            = empty

        state' | pOutReady && state == 7 = 3
               | state == 7              = 7
               | isJust sIn && not more  = 7
               | isJust sIn              = state - 1
               | otherwise               = state
