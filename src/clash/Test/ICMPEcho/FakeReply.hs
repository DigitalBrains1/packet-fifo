module Test.ICMPEcho.ScanEcho where

import Clash.Prelude
import Clash.Explicit.Testbench
import Data.Maybe
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

testEntity sIn = (sInReady, sOut)
    where
        (ramWrite, sInReady, sendReply) = scanEcho sIn ramBusy
        (ramBusy, readAddr, sOut) = fakeReply sendReply readData
        readData = blockRamPow2 (repeat undefined)
                                (register undefined readAddr)
                                (register undefined ramWrite)
testBench = done
    where
        sIn = backPresStimuliGenerator
                (packetVecToStreamVec pktI) sInReady
        (sInReady, sOut) = testEntity sIn
        expectedOutput = maybeOutputVerifier' (packetVecToStreamVec pktO)
        done = withClockResetEnable clk rst en $ expectedOutput sOut
        clk = tbSystemClockGen (not <$> done)
        rst = systemResetGen
        en = tbEnableGen
        pktI = $(listToVecTH echoReqPacket)
        pktO = $(listToVecTH echoReqPacket)

packetVecToStreamVec pkt =    map (\e -> (True, e)) (init pkt)
                           :< (False, last pkt)

main = print $ P.head $ P.dropWhile not $ sample testBench
