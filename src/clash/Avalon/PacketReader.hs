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
module Avalon.PacketReader where

import Clash.Prelude

import Avalon.Master

fifoLevelReg :: Unsigned 6
fifoLevelReg = 0
fifoDataReg :: Unsigned 6
fifoDataReg = 0x20
fifoOtherInfoReg :: Unsigned 6
fifoOtherInfoReg = 0x24

{-
 - Tag for request matching.
 -
 - The tag indicates which register the read result is for.
 -}
data AvalonTag = DataTag | OtherTag | FillTag
    deriving (Eq, Generic, NFDataX)

{-
 - Read packets from an Intel FPGA Avalon FIFO Memory Core.
 -
 - Any packets put into the FIFO are output on the `pOut` output, in the same
 - format as they are in the FIFO Memory Core. Refer to the Intel
 - documentation for the meaning of the bits in the two words `pOutData`
 - (data) and `pOutOther` (other info or packet status information).
 -
 - One element of packet data is transferred when both `pOutReady` and
 - `pOutValid` are asserted.
 -
 - `opReady`, `resReady` and `op` connect to Avalon.Master.avalonMaster.
 -
 - `packetReader` will read the fill level, and then transfer as many elements
 - as that indicates, after which it will return to reading the fill level.
 - Every element consists of two reads, first reading the fifoDataReg and then
 - reading the fifoOtherInfoReg.
 -}
packetReader
    :: HiddenClockResetEnable dom
    => "res" :::
           ( "resValid" ::: Signal dom Bool
           -- ^ Avalon result valid
           , "resTag" ::: Signal dom AvalonTag
           -- ^ Avalon result tag
           , "resData" ::: Signal dom (Unsigned 32)
           -- ^ Avalon result data
           )
    -> "pOutReady" ::: Signal dom Bool
       -- ^ Packet out ready
    -> "opReady" ::: Signal dom Bool
       -- ^ Avalon op ready
    -> ( "resReady" ::: Signal dom Bool
       -- ^ Avalon result ready
       , "pOut" :::
             ( "pOutValid" ::: Signal dom Bool
             -- ^ Packet out valid
             , "pOutData" ::: Signal dom (Unsigned 32)
             -- ^ Packet out data
             , "pOutOther" ::: Signal dom (Unsigned 32)
             -- ^ Packet out other info
             )
       , "op" :::
           ( "opValid" ::: Signal dom Bool
           -- ^ Avalon op valid
           , "opCmd" ::: Signal dom AvalonCmd
           -- ^ Avalon op command
           , "opTag" ::: Signal dom AvalonTag
           -- ^ Avalon op tag
           , "opAddr" ::: Signal dom (Unsigned 6)
           -- ^ Avalon op address
           , "opData" ::: Signal dom (Unsigned 32)
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
            = mooreB opGenT opGenO 1
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

opGenT
    :: "count (prev)" ::: Unsigned 14
    -> ( "opReady" ::: Bool
       , "resTransfer" ::: Bool
       , "resTag" ::: AvalonTag
       , "resData" ::: Unsigned 32
       )
    -> "count" ::: Unsigned 14

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

opGenO
    :: "count" ::: Unsigned 14
    -> ( "opValid" ::: Bool
       , "opTag" ::: AvalonTag
       , "opAddr" ::: Unsigned 6
       )

opGenO 0 = (False, undefined, undefined)
opGenO 1 = (True, FillTag, fifoLevelReg)
opGenO n | lsb n == 1 = (True, DataTag, fifoDataReg)
         | otherwise  = (True, OtherTag, fifoOtherInfoReg)

pOutValidT
    :: "pOutValid (prev)" ::: Bool
    -> ( "pOutReady" ::: Bool
       , "resTransfer" ::: Bool
       , "resTag" ::: AvalonTag
       )
    -> "pOutValid" ::: Bool

pOutValidT s (pOutReady, resTransfer, resTag)
    | s && pOutReady                             = False
    | not s && resTransfer && resTag == OtherTag = True
    | otherwise                                  = s
