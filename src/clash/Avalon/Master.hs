{-# LANGUAGE RankNTypes #-}

module Avalon.Master where

import Clash.Prelude
import qualified Prelude as P

import Toolbox.Fifo

{-
 - Generate proper port naming for Synthesize t_inputs annotation on top
 - entity.
 -
 - If your External Bus to Avalon Bridge is called "bridge_0", call this
 - function with the argument "bridge_0_" (note the underscore at the end). Be
 - sure to use the default name for the "external_interface" conduit in Intel
 - Platform Designer (QSys), which is "bridge_0_external_interface".
 -
 - See Test.Avalon.StreamEcho.avalonStreamEcho for an example of how to use
 - this.
 -}
avalonMasterExtInputNames :: String -> PortName
avalonMasterExtInputNames prefix
    = PortProduct "" $ P.map (PortName . (prefix P.++))
                             [ "external_interface_read_data"
                             , "external_interface_acknowledge"
                             ]
{-
 - Generate proper port naming for Synthesize t_output annotation on top
 - entity.
 -
 - If your External Bus to Avalon Bridge is called "bridge_0", call this
 - function with the argument "bridge_0_" (note the underscore at the end). Be
 - sure to use the default name for the "external_interface" conduit in Intel
 - Platform Designer (QSys), which is "bridge_0_external_interface".
 -
 - See Test.Avalon.StreamEcho.avalonStreamEcho for an example of how to use
 - this.
 -}
avalonMasterExtOutputNames :: String -> PortName
avalonMasterExtOutputNames prefix
    = PortProduct "" $ P.map (PortName . (prefix P.++))
                             [ "external_interface_address"
                             , "external_interface_write_data"
                             , "external_interface_read"
                             , "external_interface_write"
                             , "external_interface_byte_enable"
                             ]

{-
 - The Avalon-MM command (i.e., transfer) to initiate: Read or Write.
 -}
data AvalonCmd = AvalonRead | AvalonWrite deriving (Eq, Generic, NFDataX)

{-
 - Avalon-MM master with default FIFO.
 -
 - A FIFO with depth 2 is used, allowing operation without wait states.
 -}
avalonMaster
    :: ( HiddenClockResetEnable dom
       , KnownNat m
       , KnownNat n
       , NFDataX tag)
    => "avIn" :::
        (  "external_interface_read_data" ::: Signal dom (Unsigned n)
        -- ^ Avalon read data
        , "external_interface_acknowledge" ::: Signal dom Bool
        -- ^ Avalon acknowledge
        )
    -- ^ External bus signals
    -> "op" :::
        ( "opValid" ::: Signal dom Bool
        -- ^ Operation valid
        , "opCmd" ::: Signal dom AvalonCmd
        -- ^ Read or write
        , "opTag" ::: Signal dom tag
        -- ^ Passthrough tag for request matching
        , "opAddr" ::: Signal dom (Unsigned m)
        -- ^ Address
        , "opData" ::: Signal dom (Unsigned n)
        -- ^ Write data
        )
    -- ^ Interface for use of this component: operation request
    -> "resReady" ::: Signal dom Bool
    -- ^ Result ready
    -> ( "avOut" :::
           ( "external_interface_address" ::: Signal dom (Unsigned m)
           -- ^ Address
           , "external_interface_write_data" ::: Signal dom (Unsigned n)
           -- ^ Write data
           , "external_interface_read" ::: Signal dom Bool
           -- ^ Read strobe
           , "external_interface_write" ::: Signal dom Bool
           -- ^ Write strobe
           , "external_interface_byte_enable" ::: Signal dom (BitVector 4)
           -- ^ Byte enable (all bytes always enabled here)
           )
       -- ^ External bus signals
       , "opReady" ::: Signal dom Bool
       -- ^ Operation ready
       , "res" :::
           ( "resValid" ::: Signal dom Bool
           -- ^ Result valid
           , "resTag" ::: Signal dom tag
           -- ^ Passthrough tag
           , "resData" ::: Signal dom (Unsigned n)
           -- ^ Read data
           )
       -- ^ Interface for use of this component: result of operation
       )
avalonMaster = avalonMaster' (fifoP2 d1)

{-
 - Avalon-MM master with configurable input FIFO.
 -
 - The external bus signals connect to the "External Bus to Avalon Bridge"
 - instantiated by Intel Platform Designer (Qsys).
 -
 - The `op` input requests an operation to be performed. The `res` output will
 - indicate readiness when the operaton has completed, returning the `tag`
 - that was part of the operation for matching. If the operation was a read,
 - the read data is part of `res`.
 -
 - For both `op` and `res`, information is transferred in a cycle where both
 - `ready` and `valid` are `True`. NOTE that once `ready` has been asserted,
 - it can only be deasserted following a transfer!
 -}
avalonMaster'
    :: ( HiddenClockResetEnable dom
       , KnownNat m
       , KnownNat n
       , NFDataX tag)
    => "inpFifo" ::: Fifo dom (AvalonCmd, tag, Unsigned m, Unsigned n)
       -- ^ The fifo that buffers the input
    -> "avIn" :::
        (  "external_interface_read_data" ::: Signal dom (Unsigned n)
        -- ^ Avalon read data
        , "external_interface_acknowledge" ::: Signal dom Bool
        -- ^ Avalon acknowledge
        )
    -- ^ External bus signals
    -> "op" :::
        ( "opValid" ::: Signal dom Bool
        -- ^ Operation valid
        , "opCmd" ::: Signal dom AvalonCmd
        -- ^ Read or write
        , "opTag" ::: Signal dom tag
        -- ^ Passthrough tag for request matching
        , "opAddr" ::: Signal dom (Unsigned m)
        -- ^ Address
        , "opData" ::: Signal dom (Unsigned n)
        -- ^ Write data
        )
    -- ^ Interface for use of this component: operation request
    -> "resReady" ::: Signal dom Bool
    -- ^ Result ready
    -> ( "avOut" :::
           ( "external_interface_address" ::: Signal dom (Unsigned m)
           -- ^ Avalon address
           , "external_interface_write_data" ::: Signal dom (Unsigned n)
           -- ^ Avalon write data
           , "external_interface_read" ::: Signal dom Bool
           -- ^ Avalon read strobe
           , "external_interface_write" ::: Signal dom Bool
           -- ^ Avalon write strobe
           , "external_interface_byte_enable" ::: Signal dom (BitVector 4)
           -- ^ Avalon byte enable (all bytes always enabled here)
           )
       -- ^ External bus signals
       , "opReady" ::: Signal dom Bool
       -- ^ Operation ready
       , "res" :::
           ( "resValid" ::: Signal dom Bool
           -- ^ Result valid
           , "resTag" ::: Signal dom tag
           -- ^ Passthrough tag
           , "resData" ::: Signal dom (Unsigned n)
           -- ^ Read data
           )
       -- ^ Interface for use of this component: result of operation
       )

avalonMaster' inpFifo avIn op resReady
    = (avOut, opReady, res)
    where
        (opReady, opB) = inpFifo op' avAck
        (opValid, opCmd, tagIn, opAddr, opData) = op
        op' = bundle (opValid, bundle (opCmd, tagIn, opAddr, opData))
        (opBValid, opB') = unbundle opB
        (opBCmd, opBTag, opBAddr, opBData) = unbundle opB'

        (avRData, avAck) = avIn
        (avRead, avWrite)
            = unbundle $ avalonMasterGate <$> opBCmd <*> opBValid <*> resReady
        avOut = (opBAddr, opBData, avRead, avWrite, avBE)
        avBE = pure 0b1111
        res = (avAck, opBTag, avRData)

avalonMasterGate
    :: AvalonCmd
    -> Bool
    -> Bool
    -> (Bool, Bool)

avalonMasterGate opBCmd opBValid resReady = (avRead, avWrite)
    where
        avRead = opBValid && opBCmd == AvalonRead && resReady
        avWrite = opBValid && opBCmd == AvalonWrite && resReady
