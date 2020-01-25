module Test.Avalon.Master.MockSlave where

import Clash.Prelude

import Toolbox.Test

mockAvalonSlave
    :: ( HiddenClockResetEnable dom
       , KnownNat k)
    => ( Signal dom (Unsigned k)
       , Signal dom (Unsigned 32)
       , Signal dom Bool
       , Signal dom Bool
       , Signal dom (BitVector 4)
       )
    -> ( Signal dom (Unsigned 32)
       , Signal dom Bool
       )

mockAvalonSlave (addr, wdata, read, write, be)
    = ( pure 0x12345672
      , addr `seqXA` wdata `seqXA` read `seqXA` write `seqXA` be `seqXA` ack)
    where
        ack = moore cntr (== 0) (maxBound :: Index 3)
                    (read .||. write)

        cntr 0 _     = maxBound
        cntr n True  = n - 1
        cntr n False = n
