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
module Avalon.ByteStreamToPacket where

import Clash.Prelude
import Data.Maybe

import Avalon.Packet
import Toolbox.Misc

{-
 - Convert stream of bytes to Avalon packet format
 -
 - `pOut` is the format as taken by Avalon.PacketWriter (which itself is very
 - similar to the Avalon-ST packet interface). Input is a stream of byte
 - values with a marker indicating end-of-packet.
 -
 - Input is transferred whenever `isJust sIn` and `sInReady` is asserted.
 -
 - `more` indicates there are more bytes in the packet and is de-asserted for
 - the last byte in the packet. THe start of the next packet is implied to be
 - at the next byte.
 -}
byteStreamToPacket
    :: HiddenClockResetEnable dom
    => "sIn" :::
           Signal dom
               (Maybe ( "more" ::: Bool
                      , "data" ::: Unsigned 8
                      )
               )
       -- ^ Stream-in
    -> "pOutReady" ::: Signal dom Bool
       -- ^ Packet-out ready
    -> ( "sInReadu" ::: Signal dom Bool
        -- ^ Stream-in ready
       , "pOut" :::
           ( "pOutValid" ::: Signal dom Bool
           -- ^ Packet out valid
           , "pOutData" ::: Signal dom (Unsigned 32)
           -- ^ Packet out data
           , "pOutOther" ::: Signal dom (Unsigned 32)
           -- ^ Packet out other info
           )
       )

byteStreamToPacket sIn pOutReady = (sInReady, unbundle pOut)
    where
        (sInReady, pOut) = mealyB byteStreamToPacket' ( repeat 0
                                                      , True
                                                      , undefined
                                                      , 0
                                                      , 3)
                                  (bundle sIn, pOutReady)

byteStreamToPacket'
    :: Mealy ( "s" :::
                 ( "dataReg" ::: Vec 4 (Unsigned 8)
                 , "sop" ::: Bool
                 , "eop" ::: Bool
                 , "empty" ::: Unsigned 2
                 , "state" ::: Unsigned 3
                 )
             )
             ( "i" :::
                 ( "sIn" :::
                     Maybe ( "more" ::: Bool
                           , "data" ::: Unsigned 8
                           )
                 , "pOutReady" ::: Bool
                 )
             )
             ( "o" :::
                 ( "sInReady" ::: Bool
                 , "pOut" :::
                     ( "pOutValid" ::: Bool
                     , "pOutData" ::: Unsigned 32
                     , "pOutOther" ::: Unsigned 32
                     )
                 )
             )


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
               -- Note that this wraps from 0 to 7
               | isJust sIn              = state - 1
               | otherwise               = state
