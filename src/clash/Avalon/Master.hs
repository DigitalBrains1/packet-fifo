{-# LANGUAGE RankNTypes #-}

module Avalon.Master where

import Clash.Prelude
import qualified Prelude as P

import Toolbox.Fifo

avalonMasterExtInputNames prefix
    = PortProduct "" $ P.map (PortName . (prefix P.++))
                             [ "external_interface_read_data"
                             , "external_interface_acknowledge"
                             ]

avalonMasterExtOutputNames prefix
    = PortProduct "" $ P.map (PortName . (prefix P.++))
                             [ "external_interface_address"
                             , "external_interface_write_data"
                             , "external_interface_read"
                             , "external_interface_write"
                             , "external_interface_byte_enable"
                             ]

data AvalonCmd = AvalonRead | AvalonWrite deriving (Eq, Generic, NFDataX)

data AvalonMS m n tag =
        AvalonMS
            { amsRead :: Bool
            , amsWrite :: Bool
            , amsAddr :: Unsigned m
            , amsOpData :: Unsigned n
            , amsTag :: tag
            , amsResValid :: Bool
            , amsResData :: Unsigned n
            } deriving (Generic, NFDataX)

avalonMSInit = AvalonMS { amsRead = False
                        , amsWrite = False
                        , amsAddr = undefined
                        , amsOpData = undefined
                        , amsTag = undefined
                        , amsResValid = False
                        , amsResData = undefined
                        }

{-
 - Avalon busmaster with default FIFO
 -}
avalonMaster
    :: ( HiddenClockResetEnable dom
       , KnownNat m
       , KnownNat n
       , NFDataX tag)
    => ( Signal dom (Unsigned n)    -- Avalon ReadData
       , Signal dom Bool            -- Avalon Acknowledge
       )
    -- ^ External bus signals
    -> ( Signal dom Bool            -- Operation valid
       , Signal dom AvalonCmd       -- Read or write
       , Signal dom tag             -- Passthrough tag for request matching
       , Signal dom (Unsigned m)    -- Address
       , Signal dom (Unsigned n)    -- Write data
       )
    -> Signal dom Bool              -- Result ready
    -> ( ( Signal dom (Unsigned m)  -- Address
         , Signal dom (Unsigned n)  -- Write data
         , Signal dom Bool          -- Read strobe
         , Signal dom Bool          -- Write strobe
         , Signal dom (BitVector 4) -- ByteEnable
         )
       -- ^ External bus signals
       , Signal dom Bool            -- Operation ready
       , ( Signal dom Bool          -- Result valid
         , Signal dom tag           -- Passthrough tag
         , Signal dom (Unsigned n)  -- Read data
         )
       -- ^ Interface for use of this component
       )
avalonMaster = avalonMaster' (fifoP2 d1)

{-
 - Avalon busmaster with configurable input FIFO
 -}
avalonMaster'
    :: ( HiddenClockResetEnable dom
       , KnownNat m
       , KnownNat n
       , NFDataX tag)
    => Fifo dom (AvalonCmd, tag, Unsigned m, Unsigned n)
       -- ^ The fifo that buffers the input
    -> ( Signal dom (Unsigned n)    -- Avalon ReadData
       , Signal dom Bool            -- Avalon Acknowledge
       )
    -- ^ External bus signals
    -> ( Signal dom Bool            -- Operation valid
       , Signal dom AvalonCmd       -- Read or write
       , Signal dom tag             -- Passthrough tag for request matching
       , Signal dom (Unsigned m)    -- Address
       , Signal dom (Unsigned n)    -- Write data
       )
    -> Signal dom Bool              -- Result ready
    -> ( ( Signal dom (Unsigned m)  -- Address
         , Signal dom (Unsigned n)  -- Write data
         , Signal dom Bool          -- Read strobe
         , Signal dom Bool          -- Write strobe
         , Signal dom (BitVector 4) -- ByteEnable
         )
       -- ^ External bus signals
       , Signal dom Bool            -- Operation ready
       , ( Signal dom Bool          -- Result valid
         , Signal dom tag           -- Passthrough tag
         , Signal dom (Unsigned n)  -- Read data
         )
       -- ^ Interface for use of this component
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
            = unbundle $ avalonMasterGate <$> opCmd <*> opBValid <*> resReady
        avOut = (opBAddr, opBData, avRead, avWrite, avBE)
        avBE = pure 0b1111
        res = (avAck, opBTag, avRData)

avalonMasterGate opCmd opBValid resReady = (avRead, avWrite)
    where
        avRead = opCmd == AvalonRead && opBValid && resReady
        avWrite = opCmd == AvalonWrite && opBValid && resReady
