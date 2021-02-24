{-
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
module Avalon.PacketWriter where

import Clash.Prelude

import Avalon.Master

fifoDataReg :: Unsigned 3
fifoDataReg = 0x0
fifoOtherInfoReg :: Unsigned 3
fifoOtherInfoReg = 0x4

{-
 - The AvalonMasterOp for an Intel FPGA Avalon FIFO Memory Core as output.
 -}
type PacketWriterOp dom = AvalonMasterOp dom () 3 32

type PacketWriterExtInput dom = AvalonMasterExtInput dom 32

type PacketWriterExtOutput dom = AvalonMasterExtOutput dom 3 32

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
       , "op" ::: PacketWriterOp dom
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
