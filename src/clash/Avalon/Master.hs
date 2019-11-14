module Avalon.Master where

import Clash.Prelude
import qualified Prelude as P

{-
 - Open questions:
 -
 - - state updating: are non-update fields muxed by the if-then as well? Does
 -   this hurt?
 -}

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

avalonMaster
    :: ( HiddenClockResetEnable dom
       , KnownNat m
       , KnownNat n
       , NFDataX tag)
    => ( Signal dom (Unsigned n)    -- Avalon ReadData
       , Signal dom Bool            -- Avalon Acknowledge
       )
    -- ^ External bus signals
    -> Signal dom Bool              -- Result ready
    -> ( Signal dom Bool            -- Operation valid
       , Signal dom AvalonCmd       -- Read or write
       , Signal dom tag             -- Passthrough tag for request matching
       , Signal dom (Unsigned m)    -- Address
       , Signal dom (Unsigned n)    -- Write data
       )
    -> ( ( Signal dom (Unsigned m)  -- Address
         , Signal dom (Unsigned n)  -- Write data
         , Signal dom Bool          -- Read strobe
         , Signal dom Bool          -- Write strobe
         , Signal dom (BitVector 4) -- ByteEnable
         )
       -- External bus signals
       -- Interface for use of this component
       , Signal dom Bool            -- Operation ready
       , ( Signal dom Bool          -- Result valid
         , Signal dom tag           -- Passthrough tag
         , Signal dom (Unsigned n)  -- Read data
         )
       )

{-
 - Notes on principle of operation:
 -
 - Bus is busy as long as avRead or avWrite is asserted. We are ready for the
 - next operation when the bus is idle and our output registers available.
 -}
avalonMaster avIn resReady op
    = (avOut, opReady, res)
    where
        avOut = (avAddr, avWData, avRead, avWrite, avBE)
        res = (resValid, tagOut, resData)

        opReady = not <$> (resValid .||. avRead .||. avWrite)
        avAddr = amsAddr <$> state
        avWData = amsOpData <$> state
        avRead = amsRead <$> state
        avWrite = amsWrite <$> state
        resValid = amsResValid <$> state
        tagOut = amsTag <$> state
        resData = amsResData <$> state
        avBE = pure 0b1111

        state = moore avalonMasterT id avalonMSInit
                      (bundle (bundle avIn, opReady, resReady, bundle op))

avalonMasterT
    :: ( KnownNat m
       , KnownNat n
       , NFDataX tag
       )
    => AvalonMS m n tag
    -> ( ( Unsigned n   -- Avalon ReadData
         , Bool         -- Avalon Acknowledge
         )
       , Bool           -- Operation ready
       , Bool           -- Result ready
       , ( Bool         -- Operation valid
         , AvalonCmd    -- Read or write
         , tag          -- Passthrough tag for request matching
         , Unsigned m   -- Address
         , Unsigned n   -- Write data
         )
       )
    -> AvalonMS m n tag
avalonMasterT state
    (avIn, opReady, resReady, op)
    = state''
    where
        (avRData, avAck) = avIn
        (opValid, opCmd, tagIn, opAddr, opData) = op
        state' =
            if opValid && opReady then
                state { amsRead = opCmd == AvalonRead
                      , amsWrite = opCmd == AvalonWrite
                      , amsAddr = opAddr
                      , amsOpData = opData
                      , amsTag = tagIn
                      }
            else
                state

        state'' =
            if avAck then
                state' { amsRead = False
                       , amsWrite = False
                       , amsResValid = True
                       , amsResData = avRData
                       }
            else
                state' { amsResValid = amsResValid state && (not resReady) }
