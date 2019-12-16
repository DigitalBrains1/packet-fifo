module Avalon.PacketReader where

import Clash.Prelude

import Avalon.Master

fifoLevelReg = 0
fifoDataReg = 0x20
fifoOtherInfoReg = 0x24

data AvalonTag = DataTag | OtherTag | FillTag
    deriving (Eq, Generic, NFDataX)

packetReader
    :: HiddenClockResetEnable dom
    => ( Signal dom Bool
       -- ^ Avalon result valid
       , Signal dom AvalonTag
       -- ^ Avalon result tag
       , Signal dom (Unsigned 32)
       -- ^ Avalon result data
       )
    -> Signal dom Bool
       -- ^ Packet out ready
    -> Signal dom Bool
       -- ^ Avalon op ready
    -> ( Signal dom Bool
       -- ^ Avalon result ready
       , ( Signal dom Bool
         -- ^ Packet out valid
         , Signal dom (Unsigned 32)
         -- ^ Packet out data
         , Signal dom (Unsigned 32)
         -- ^ Packet out other info
         )
       , ( Signal dom Bool
         -- ^ Avalon op valid
         , Signal dom AvalonCmd
         -- ^ Avalon op command
         , Signal dom AvalonTag
         -- ^ Avalon op tag
         , Signal dom (Unsigned 6)
         -- ^ Avalon op address
         , Signal dom (Unsigned 32)
         -- ^ Avalon op data
         )
       )

packetReader res pOutReady opReady
    = (resReady, pOut, op)
    where
        (resValid, resTag, resData) = res
        pOut = (pOutValid, pOutData, pOutOther)
        op = (opValid, pure AvalonRead, opTag, opAddr, pure undefined)
        (opValid, opTag, opAddr)
            = mooreB opGenT opGenO (1 :: Unsigned 14)
                     (opReady, resTransfer, resTag, resData)
        pOutValid
            = moore pOutValidT id False
                    (bundle (pOutReady, resTransfer, resTag))
        resReady = not <$> pOutValid
        resTransfer = resReady .&&. resValid
        pOutData
             = regEn undefined (resTransfer .&&. ((== DataTag) <$> resTag))
                    resData
        pOutOther
            = regEn undefined (resTransfer .&&. ((== OtherTag) <$> resTag))
                    resData

opGenT count (opReady, resTransfer, resTag, resData)
    = count'
    where
        count' | count > 0 = countDown
               | otherwise = waitFill

        countDown | opReady   = count - 1
                  | otherwise = count

        waitFill
            | resTransfer && resTag == FillTag
                = unpack (pack (truncateB resData :: Unsigned 13) ++# 1)
            | otherwise
                = count

opGenO 0 = (False, undefined, undefined)
opGenO 1 = (True, FillTag, fifoLevelReg)
opGenO n | lsb n == 1 = (True, DataTag, fifoDataReg)
         | otherwise  = (True, OtherTag, fifoOtherInfoReg)

pOutValidT s (pOutReady, resTransfer, resTag)
    | s && pOutReady                             = False
    | not s && resTransfer && resTag == OtherTag = True
    | otherwise                                  = s
