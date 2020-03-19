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
 -
 - TODO: Make it work for `cycles` == d0 (XXX: change `ack` so it's only
 -       active when an actual request is issued)
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
      , addr `seqXA` wdata `seqXA` read `seqXA` write `seqXA` be `seqXA` ack)
    where
        ack = (== 0) <$> cnt

        cnt :: Signal dom (Index d)
        cnt = regEn maxBound (read .||. write) $ satPred SatWrap <$> cnt
