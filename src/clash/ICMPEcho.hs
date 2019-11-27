module ICMPEcho where

import Clash.Prelude
import Data.Maybe
import Debug.Trace (trace)

import qualified Prelude as P

scanEcho
    :: HiddenClockResetEnable dom
    => Signal dom (Maybe (Bool, Unsigned 8))
       -- ^ Stream-in
    -> Signal dom Bool
       -- ^ RAM busy
    -> ( Signal dom (Maybe (Unsigned 11, Unsigned 8))
         -- ^ RAM write
       , Signal dom Bool
         -- ^ Stream-in ready
       , Signal dom (Maybe (Unsigned 11))
         -- Send reply
       )

scanEcho sIn ramBusy = mealyB scanEcho' (0, True) (sIn, ramBusy)

scanEcho' (cnt, isPing) (sIn, ramBusy)
    = ((cnt', isPing'), (ramWrite, sInReady, sendReply))
    where
        (sInMore, sInData) = fromJust sIn

        ramWrite | sIn == Nothing = Nothing
                 | otherwise      = Just (cnt, sInData)

        isPing' | cnt' == 0                                    = True
                |    sIn == Nothing
                  || cnt >= minIcmpEchoLength
                  || sInData .&. vrfyMask!!cnt == vrfyVal!!cnt = isPing
                | otherwise                                    = False

        cnt' |    not sInReady
               || sIn == Nothing = cnt
             | not sInMore       = 0
             | otherwise         = cnt + 1

        sInReady = not (cnt == 0 && ramBusy)

        sendReply | cnt' /= 0                              = Nothing
                  | isPing && cnt >= (minIcmpEchoLength-1) = Just cnt
                  | otherwise                              = Nothing

minIcmpEchoLength
    =   14    -- Ethernet header
      + 20    -- IPv4 header
      + 8     -- Minimum ICMP packet length

vrfyMask
    = $(listToVecTH
            ([ 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00
             , 0x00, 0x00, 0x00, 0x00, 0xff, 0xff
             -- ^ Ethernet header
             , 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
             , 0x00, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
             , 0xff, 0xff, 0xff, 0xff
             -- ^ IPv4 header
             , 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
             -- ^ ICMP header
             ] :: [Unsigned 8]))

vrfyVal
    = $(listToVecTH
            ([ 0x52, 0x54, 0x00, 0xeb, 0x9b, 0xd0, 0x00, 0x00
             , 0x00, 0x00, 0x00, 0x00, 0x08, 0x00
             -- ^ ethernet header
             , 0x45, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
             , 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
             , 0x0a, 0x00, 0x00, 0x02
             -- ^ ipv4 header
             , 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
             -- ^ icmp header
             ] :: [Unsigned 8]))

fakeReply
    :: HiddenClockResetEnable dom
    => Signal dom (Maybe (Unsigned 11))
       -- ^ Send reply
    -> Signal dom (Unsigned 8)
       -- ^ RAM read data
    -> ( Signal dom Bool
         -- ^ RAM busy
       , Signal dom (Unsigned 11)
         -- ^ RAM read address
       , Signal dom (Maybe (Bool, Unsigned 8))
         -- ^ Stream-out
       )

fakeReply sendReply readData
    = mealyB fakeReply' fakeReplyIS
             (sendReply, readData)

fakeReplyIS = ( cntInit :: Unsigned 12
              , 0       :: Unsigned 12
              , 0       :: Unsigned 8
              )

fakeReply' (cnt, total, prevB) (sendReply, readData)
    = ((cnt', total', prevB'), (ramBusy, readAddr, sOut))
    where
        ramBusy  = cnt /= cntInit
        readAddr = truncateB cnt
        sOut | sOutValid = Just (more, sData)
             | otherwise = Nothing
        more = cnt < total + 3
        sOutValid = not (cnt >= cntInit && cnt < cntInit + 3)

        prevB' | cnt == 39 = resize cksum
               | otherwise = readData

        total' | total > 0 && not more = 0
               | otherwise              = maybe total resize sendReply

        cnt' | total == 0                           = cntInit
             | cnt < total + 3                      = cntAdvance
             | otherwise                            = cntInit

        cntAdvance
            = case cnt of
                11 ->  0   -- Ethernet src -> Ethernet dst MAC
                5  -> 12   -- Ethernet dst MAC -> Ethertype
                25 -> 30   -- IPv4 Header sum -> dest IP
                33 -> 26   -- Dest IP -> src IP
                29 -> 34   -- Src IP -> ICMP
                _  -> cnt + 1

        -- Note three cycle delay of readData vs. cnt
        sData
            = case cnt of
                37 -> 0                   -- ICMP type
                39 -> resize $ cksum `shiftR` 8
                _  -> prevB

        cksum = bitCoerce (prevB :> readData :> Nil) ~+~ 0x0800 :: Unsigned 16

(~+~) :: KnownNat n
      => Unsigned n
      -> Unsigned n
      -> Unsigned n
a ~+~ b = truncateB summed + resize (bitCoerce carry)
    where
        summed = a `add` b
        carry = msb summed

cntInit = 6 -- Ethernet source MAC
