{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.ICMPEcho.ScanEcho where

import Clash.Prelude
import qualified Clash.Explicit.Prelude as CEP
import Clash.Explicit.Testbench
import Data.Maybe
import qualified Data.Text.IO as TIO
import Debug.Trace
import qualified Prelude as P

import ICMPEcho
import Test.ICMPEcho.TestPackets

backPresStimuliGenerator stim ready
    = moore bpsgT (bpsgO stim) (0, False) ready
    where
        bpsgT s@(_, True ) _                     = s
        bpsgT s@(c, False) False                 = s
        bpsgT   (c, False) True  | c == maxBound = (c  , True)
                                 | otherwise     = (c+1, False)

        bpsgO
            :: KnownNat n
            => Vec n a
            -> (Index n, Bool)
            -> Maybe a
        bpsgO stim (c, False) = Just (stim !! c)
        bpsgO _    (_, True ) = Nothing

maybeOutputVerifier' samples = moore (movT samples) movO (0, False)
    where
        movT :: (KnownNat n, Eq a, ShowX a)
             => Vec n a
             -> (Index n, Bool)
             -> Maybe a
             -> (Index n, Bool)
        movT _       s@(_  , True ) _              = s
        movT _       s              Nothing        = s
        movT samples   (cnt, False) (Just checked)
            = if checked == expected then nextS
                else trace (P.concat [ "\nElement "
                                     , show cnt
                                     , ", "
                                     , "\nexpected value: "
                                     , showX expected
                                     , ", not equal to actual value: "
                                     , showX checked
                                     ]) nextS
            where
                nextS | cnt == maxBound = (maxBound, True )
                      | otherwise       = (cnt + 1 , False)

                expected = samples !! cnt

        movO (_, done) = done

stallStream
    :: ( HiddenClockResetEnable dom
       , KnownNat n, ShowX a
       )
    => Vec n (Int, Int)
    -> Signal dom (Maybe a)
    -> ( Signal dom Bool
       , Signal dom (Maybe a)
       )

stallStream ss sIn = unbundle $ mealy (stallStreamT ss) (0,0,1) sIn

stallStreamT
    :: (KnownNat n, ShowX a)
    => Vec n (Int, Int)
    -> ( Index n
       , Int
       , Int
       )
    -> Maybe a
    -> ( ( Index n
         , Int
         , Int
         )
       , ( Bool
         , Maybe a
         )
       )

{-
 - Takes a vector (ss) of elements (takenum, stallnum) which means: take
 - takenum elements from the stream and then stall for stallnum cycles.
 - Misbehaves for stallnum == 0, don't do that.
 -}
stallStreamT ss (ind, taken, stalled) sIn =
    ((ind', taken', stalled'), (not stall, sOut))
    where
        (takenum, stallnum) = ss !! ind

        stall = taken == takenum

        next = stalled == stallnum

        sOut | stall     = Nothing
             | otherwise = sIn

        taken' | next        = 0
               | isJust sOut = taken + 1
               | otherwise   = taken

        stalled' | next      = 1
                 | stall     = stalled + 1
                 | otherwise = stalled

        ind' | ind == maxBound = maxBound
             | next            = ind + 1
             | otherwise       = ind

extractState
    :: ( HiddenClockResetEnable dom
       , NFDataX s
       )
    => (s -> i -> (s,o))
    -> s
    -> Signal dom s
    -> Signal dom i
    -> (Signal dom s, Signal dom o)
extractState mealyT is sI i = (sE,o)
    where
        s = register is sI
        (sE, o) = unbundle $ mealyT <$> s <*> i

traceFakeReply
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

traceFakeReply sendReply readData = (ramBusy', readAddr', sOut')
    where
        (s', o) =   extractState fakeReply' fakeReplyIS s''
                  $ bundle (sendReply', readData)
        (cnt, total, prevB) = unbundle s'
        s'' = bundle (cnt', total', prevB')
        (ramBusy, readAddr, sOut) = unbundle o

        cnt' = traceSignal1 "cnt" cnt
        total' = traceSignal1 "total" total
        prevB' = traceSignal1 "prevB" prevB
        ramBusy' = traceSignal1 "ramBusy" ramBusy
        readAddr' = traceSignal1 "readAddr" readAddr
        sOut' = traceSignal1 "sOut" sOut
        sendReply' = traceSignal1 "sendReply" sendReply

testEntity
    :: KnownDomain dom
    => Clock dom
    -> Reset dom
    -> Enable dom
    -> Signal dom (Maybe (Bool, Unsigned 8))
    -> Signal dom Bool
    -> ( Signal dom Bool
       , Signal dom (Maybe (Bool, Unsigned 8))
       )
testEntity clk rst en sIn sOutReady = (sInReady, sOut)
    where
        (ramWrite, sInReady, sendReply)
            = withClockResetEnable clk rst en scanEcho sIn ramBusy
        (ramBusy, readAddr, sOut)
            = withClock clk $ withReset rst $ withEnable sOutEn
              traceFakeReply sendReply readData'

        readData = CEP.asyncRamPow2 clk clk en readAddr' ramWrite

        readData' = CEP.register clk rst sOutEn 0 readData
        readAddr' = CEP.register clk rst sOutEn 0 readAddr
        sOutEn = CEP.enable en sOutReady

testBench :: Signal System Bool
testBench = done
    where
        sIn = withClockResetEnable clk rst en $ backPresStimuliGenerator
                (packetVecToStreamVec pktI) sInReady
        (sInReady, sOut) = testEntity clk rst en sIn sOutReady'
        (sOutReady, sOut')
            = withClockResetEnable clk rst en
              $ stallStream $(listToVecTH
                                ([ (1,1), (1,2), (1,3), (1,4), (1,5)
                                 , (2,1), (2,2), (2,3), (2,4), (2,5)
                                 , (3,1), (3,2), (3,3), (3,4), (3,5)
                                 , (4,1), (4,2), (4,3), (4,4), (4,5)
                                 , (5,1), (5,2), (5,3), (5,4), (5,5)
                                 ] :: [(Int,Int)]))
                             sOut
        sOutReady' = traceSignal1 "sOutReady" sOutReady
        expectedOutput = maybeOutputVerifier' (packetVecToStreamVec pktO)
        done = withClockResetEnable clk rst en $ expectedOutput sOut'
        clk = tbSystemClockGen (not <$> done)
        rst = systemResetGen
        en = tbEnableGen
        pktI = $(listToVecTH echoReqPacket)
        pktO = $(listToVecTH echoReplPacket)

packetVecToStreamVec pkt =    map (\e -> (True, e)) (init pkt)
                           :< (False, last pkt)

runTB = print $ P.head $ P.dropWhile not $ sample testBench

makeVCD = do
            vcd <-
                dumpVCD
                    (0,2000)
                    testBench
                    [ "cnt"
                    , "total"
                    , "prevB"
                    , "ramBusy"
                    , "readAddr"
                    , "sOut"
                    , "sOutReady"
                    , "sendReply"
                    ]
            case vcd of
               Left s  -> putStrLn s
               Right d -> TIO.writeFile "fakereply.vcd" d
