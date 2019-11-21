module Avalon.Packet where

import Clash.Prelude

-- | Start of packet marker (bit position)
fifoInfoSOP = 0
-- | End of packet marker (bit position)
fifoInfoEOP = 1

-- | Extract "empty" field from fifoOtherInfo
fromFifoInfoEmpty :: Unsigned 32
                  -> Unsigned 5
fromFifoInfoEmpty = truncateB . (`shiftR` 2)

-- | Set "empty" field in fifoOtherInfo

toFifoInfoEmpty :: Unsigned 5
                -> Unsigned 32
toFifoInfoEmpty = zeroExtend . (`shiftL` 2)
