{-
 - Copyright (c) 2012-2015 University of Twente
 - Copyright (c) 2015,2017 Peter Lebbing <peter@digitalbrains.com>
 - Copyright (c) 2020 QBayLogic B.V.
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
 -
 - This file is based on examples/Fifo.hs from the CλaSH source code.
 -}
{-# LANGUAGE RankNTypes #-}

module Toolbox.Fifo where

import Clash.Prelude

type Fifo dom e
    = ( HiddenClockResetEnable dom
      , NFDataX e)
    => Signal dom (Bool, e)
    -> Signal dom Bool
    -> (Signal dom Bool, Signal dom (Bool, e))

{-
 - An initially-empty FIFO with a power-of-two depth
 -
 - Because of the power-of-two constraint, combinatorial logic is saved by
 - only needing addition for pointer updates.
 -
 - Inputs:
 -      (inValid, inData): Data to be inserted. Insertion occurs when both
 -          inValid and inReady.
 -      outReady: True when data can be transferred out of the FIFO. Element
 -          removal occurs when both outReady and outValid.
 -
 - Outputs (inReady, (outValid, outData)):
 -      inReady: FIFO not full. See inValid.
 -      outValid: FIFO not empty. See outReady.
 -      outData: When outValid, contains the data at the head of the FIFO. The
 -      data will stay valid and constant up to and including the cycle that
 -          outReady is asserted.
 -}
fifoP2
    :: ( HiddenClockResetEnable dom
       , KnownNat n
       , NFDataX e)
    => SNat n
    -> Signal dom (Bool, e)
    -> Signal dom Bool
    -> (Signal dom Bool, Signal dom (Bool, e))


fifoP2 n inp outReady
    = (inpReady, bundle (outValid, outData))
    where
        (inpReady, outValid, write, rInd, wInd)
            = mealyB (fifoP2' n) (0, 0) (inpValid, outReady)
        (inpValid, inpData) = unbundle inp
        outData
            = readNew (blockRamU NoClearOnReset (nPow2 n) undefined)
                rInd mWrite
        nPow2
            :: (KnownNat k, KnownNat (2^k))
            => SNat k -> SNat (2^k)
        nPow2 = const SNat
        mWrite = mux write (Just <$> bundle (wInd, inpData)) (pure Nothing)

fifoP2'
    :: (KnownNat n, KnownNat (n+1))
    => SNat n
    -> (Unsigned (n+1), Unsigned (n+1))
    -> (Bool, Bool)
    -> ( (Unsigned (n+1), Unsigned (n+1))
       , (Bool, Bool, Bool, Unsigned n, Unsigned n)
       )

fifoP2' _ (rPntr, wPntr) (inpValid, outReady)
    = ( (rPntr', wPntr')
      , (inpReady, outValid, write, rInd', wInd)
      )
    where
        chop
            :: (KnownNat (k+1), KnownNat k)
            => Unsigned (k+1)
            -> Unsigned k
        chop = resize
        rInd = chop rPntr
        wInd = chop wPntr
        indEq = rInd == wInd
        flagEq = msb rPntr == msb wPntr
        inpReady = not indEq || flagEq
        outValid = not (indEq && flagEq)
        write = inpReady && inpValid
        read  = outReady && outValid
        wPntr' | write     = wPntr + 1
               | otherwise = wPntr
        rPntr' | read      = rPntr + 1
               | otherwise = rPntr
        rInd' = chop rPntr'
