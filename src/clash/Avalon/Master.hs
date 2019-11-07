module Avalon.Master where

import Clash.Prelude

{-
 - Open questions:
 -
 - - state updating: are non-update fields muxed by the if-then as well? Does
 -   this hurt?
 -}

data AvalonOp = AvalonRead | AvalonWrite deriving (Eq, Generic, NFDataX)

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
    -- External bus signals
    => Signal dom (Unsigned n)      -- Avalon ReadData
    -> Signal dom Bool              -- Avalon Acknowledge
    -- Interface for use of this component
    -> Signal dom Bool              -- Operation valid
    -> Signal dom Bool              -- Result ready
    -> Signal dom tag               -- Passthrough tag for request matching
    -> Signal dom AvalonOp          -- Read or write
    -> Signal dom (Unsigned m)      -- Address
    -> Signal dom (Unsigned n)      -- Write data
       -- External bus signals
    -> ( Signal dom (Unsigned m)    -- Address
       , Signal dom (Unsigned n)    -- Write data
       , Signal dom Bool            -- Read strobe
       , Signal dom Bool            -- Write strobe
       , Signal dom (BitVector 4)   -- ByteEnable
       -- Interface for use of this component
       , Signal dom Bool            -- Result valid
       , Signal dom Bool            -- Operation ready
       , Signal dom tag             -- Passthrough tag
       , Signal dom (Unsigned n)    -- Read data
       )

{-
 - Notes on principle of operation:
 -
 - Bus is busy as long as avRead or avWrite is asserted. We are ready for the
 - next operation when the bus is idle and our output registers available.
 -}
avalonMaster avRData avAck opValid resReady tagIn op opAddr opData
    = ( avAddr, avWData, avRead, avWrite, avBE, resValid, opReady
      , tagOut , resData)
    where
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
                      (bundle ( avRData, avAck, opValid, opReady, resReady
                              , tagIn, op, opAddr, opData))

avalonMasterT
    :: ( KnownNat m
       , KnownNat n
       , NFDataX tag
       )
    => AvalonMS m n tag
    -> ( Unsigned n     -- Avalon ReadData
       , Bool           -- Avalon Acknowledge
       , Bool           -- Operation valid
       , Bool           -- Operation ready
       , Bool           -- Result ready
       , tag            -- Passthrough tag for request matching
       , AvalonOp       -- Read or write
       , Unsigned m     -- Address
       , Unsigned n     -- Write data
       )
    -> AvalonMS m n tag
avalonMasterT state
    (avRData, avAck, opValid, opReady, resReady, tagIn, op, opAddr, opData)
    = state''
    where
        state' =
            if opValid && opReady then
                state { amsRead = op == AvalonRead
                      , amsWrite = op == AvalonWrite
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

amB i = bundle o
    where
        (avRData, avAck, opValid, resReady, tagIn, op, opAddr, opWData)
            = unbundle i
        o = avalonMaster avRData avAck opValid resReady tagIn op opAddr
            opWData

idleInput :: (Unsigned 32, Bool, Bool, Bool, Index 3, AvalonOp, Unsigned 6, Unsigned 32)
idleInput = ( undefined, False, False, False, undefined, AvalonRead, undefined
            , undefined)
