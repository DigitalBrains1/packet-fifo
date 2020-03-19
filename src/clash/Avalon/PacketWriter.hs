module Avalon.PacketWriter where

import Clash.Prelude

import Avalon.Master

fifoDataReg :: Unsigned 3
fifoDataReg = 0x0
fifoOtherInfoReg :: Unsigned 3
fifoOtherInfoReg = 0x4

{-
 - Write packets to an Intel FPGA Avalon FIFO Memory Core.
 -
 - Data on `pIn` is pushed onto the FIFO. `pIn` is in the same format as the
 - data in the FIFO Memory Core. Refer to the Intel documentation for the
 - meaning of the bits in the two words `pInData` (data) and `pInOther`
 - (other info or packet status information).
 -
 - One element of packet data is transferred when both `pInReady` and
 - `pInValid` are asserted.
 -
 - `opReady` and `op` connect to Avalon.Master.avalonMaster. Its `res` is
 - unused.
 -
 - When backpressure is enabled in Intel Platform Designer (Qsys),
 - `packetWriter` and `avalonMaster` will be blocked when the FIFO is full.
 - This means `pInReady` will simply not be asserted until the FIFO has some
 - room again.
 -}
packetWriter
    :: forall dom . HiddenClockResetEnable dom
    => "pIn" :::
           ( "pInValid" ::: Signal dom Bool
           -- ^ Packet in valid
           , "pInData" ::: Signal dom (Unsigned 32)
           -- ^ Packet in data
           , "pInOther" ::: Signal dom (Unsigned 32)
           -- ^ Packet in other info
           )
    -> "opReady" ::: Signal dom Bool
       -- ^ Avalon op ready
    -> ( "pInReady" ::: Signal dom Bool
       -- ^ Packet in ready
       , "op" :::
           ( "opValid" ::: Signal dom Bool
           -- ^ Avalon op valid
           , "opCmd" ::: Signal dom AvalonCmd
           -- ^ Avalon op command
           , "opTag" ::: Signal dom ()
           -- ^ Avalon op tag
           , "opAddr" ::: Signal dom (Unsigned 3)
           -- ^ Avalon op address
           , "opData" ::: Signal dom (Unsigned 32)
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

        sequence :: Signal dom (Index 3)
        sequence
            = regEn 0 (pInTransfer .||. opTransfer)
                (satSucc SatWrap <$> sequence)

        seq2op 0 d o = (undefined, undefined)
        seq2op 1 d o = (fifoOtherInfoReg, o)
        seq2op 2 d o = (fifoDataReg, d)
