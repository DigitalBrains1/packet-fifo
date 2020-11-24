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
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Avalon.Master.MockSlave where

import Clash.Prelude

import Toolbox.Test

{-
 - Mockup for External Bus to Avalon Bridge
 -
 - This component can be used for pure Haskell simulation. It pvorides an
 - interface similar to the Intel Platform Designer (Qsys) External Bus to
 - Avalon Bridge IP block. This mockup merely asserts
 - `external_interface_acknowledge` in response to either
 - `external_interface_read` or `external_interface_write`. Input `cycles`
 - determines how many clock cycles one request takes.
 -
 - All input signals are fully evaluated (reduced to normal form).
 -}
mockAvalonSlave
    :: forall dom d k .
       ( HiddenClockResetEnable dom
       , KnownNat d
       , KnownNat k
       , 1 <= d
       )
    => "cycles" ::: SNat d
    -> ( "external_interface_address" ::: Signal dom (Unsigned k)
       , "external_interface_write_data" ::: Signal dom (Unsigned 32)
       , "external_interface_read" ::: Signal dom Bool
       , "external_interface_write" ::: Signal dom Bool
       , "external_interface_byte_enable" ::: Signal dom (BitVector 4)
       )
    -> ( "external_interface_read_data" ::: Signal dom (Unsigned 32)
       , "external_interface_acknowledge" ::: Signal dom Bool
       )

mockAvalonSlave _ (addr, wdata, read, write, be)
    = ( pure 0x12345672
      , addr `seqXA` wdata `seqXA` be `seqXA` ack)
    where
        ack = ((== 0) <$> cnt) .&&. (read .||. write)

        cnt :: Signal dom (Index d)
        cnt = regEn maxBound (read .||. write) $ satPred SatWrap <$> cnt
