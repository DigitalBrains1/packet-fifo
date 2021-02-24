{-
 - This design will echo packets back to the HPS, but will mask out anything but
 - the first 8-bit symbol in every 32-bit beat. This is useful to verify
 - endianness is correctly handled. Only the first, the fifth, the ninth,
 - etcetera byte of the returned packet matches the sent packet, the others are
 - always zero.
 -
 - Also see Test.Avalon.PacketEcho.Common .
 -
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
module Test.Avalon.PacketEcho.OnlyFirst where

import Clash.Prelude

import Test.Avalon.Common
import Test.Avalon.PacketEcho.Common

avalonPacketEcho :: PacketFifoTopEntity
avalonPacketEcho = packetFifoTopEntity $ avalonPacketEchoXfm (.&. 0xff000000)
{-# ANN avalonPacketEcho packetFifoSynthesize #-}
{-# NOINLINE avalonPacketEcho #-}
