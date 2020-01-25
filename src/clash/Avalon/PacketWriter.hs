module Avalon.PacketWriter where

import Clash.Prelude

import Avalon.Master

fifoDataReg = 0x0
fifoOtherInfoReg = 0x4

fifoInfoSOP = 1
fifoInfoEOP = 2

packetWriter
    :: HiddenClockResetEnable dom
    => ( Signal dom Bool
       -- ^ Packet in valid
       , Signal dom (Unsigned 32)
       -- ^ Packet in data
       , Signal dom (Unsigned 32)
       -- ^ Packet in other info
       )
    -> Signal dom Bool
       -- ^ Avalon op ready
    -> ( Signal dom Bool
       -- ^ Packet in ready
       , ( Signal dom Bool
         -- ^ Avalon op valid
         , Signal dom AvalonCmd
         -- ^ Avalon op command
         , Signal dom ()
         -- ^ Avalon op tag
         , Signal dom (Unsigned 3)
         -- ^ Avalon op address
         , Signal dom (Unsigned 32)
         -- ^ Avalon op data
         )
       )

packetWriter pIn opReady = (pInReady, op)
    where
        op = (opValid, pure AvalonWrite, pure (), opAddr, opData)
        (pInValid, pInData, pInOther) = pIn

        pInTransfer = pInReady .&&. pInValid
        opTransfer = opReady .&&. opValid

        dataReg = regEn undefined pInTransfer pInData
        otherReg = regEn undefined pInTransfer pInOther

        pInReady = (== 0) <$> sequence
        opValid = not <$> pInReady
        (opAddr, opData) = unbundle $ seq2op <$> sequence <*> dataReg
                                             <*> otherReg

        sequence = moore seqT id (0 :: Index 3) (pInTransfer .||. opTransfer)

        seqT s False                 = s
        seqT s True  | s == maxBound = minBound
                     | otherwise     = s+1

        seq2op 0 d o = (undefined, undefined)
        seq2op 1 d o = (fifoOtherInfoReg, o)
        seq2op 2 d o = (fifoDataReg, d)
