{-# LANGUAGE ScopedTypeVariables #-}
module Test.Avalon.Master.MockSlave where

import Clash.Prelude

import Toolbox.Test

mockAvalonSlave
    :: forall dom d k .
       ( HiddenClockResetEnable dom
       , KnownNat d
       , KnownNat k
       , 1 <= d
       )
    => SNat d
    -> ( Signal dom (Unsigned k)
       , Signal dom (Unsigned 32)
       , Signal dom Bool
       , Signal dom Bool
       , Signal dom (BitVector 4)
       )
    -> ( Signal dom (Unsigned 32)
       , Signal dom Bool
       )

mockAvalonSlave _ (addr, wdata, read, write, be)
    = ( pure 0x12345672
      , addr `seqXA` wdata `seqXA` read `seqXA` write `seqXA` be `seqXA` ack)
    where
        ack = moore cntr (== 0) (maxBound :: Index d)
                    (read .||. write)

        cntr n False = n
        cntr n True  = satPred SatWrap n
