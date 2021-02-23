{-
 - Copyright (c) 2019-2021 QBayLogic B.V.
 - All rights reserved.
 -
 - Redistribution and use in source and binary forms, with or without
 - modification, are permitted provided that the following conditions are met:
 -
 - 1. Redistributions of source code must retain the above copyright notice,
 - this list of conditions and the following disclaimer.
 -
 - 2. Redistributions in binary form must reproduce the above copyright notice,
 - this list of conditions and the following disclaimer in the documentation
 - and/or other materials provided with the distribution.
 -
 - THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 - AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 - IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 - ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 - LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 - CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 - SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 - INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 - CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 - ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 - POSSIBILITY OF SUCH DAMAGE.
 -}
module Toolbox.Test where

import qualified Clash.Explicit.Prelude as CEP
import Clash.Prelude
import qualified Data.Text.IO as TIO
import Data.Maybe
import Debug.Trace
import qualified Prelude as P
import Type.Reflection (Typeable)

{----------------------------------------------------------------------------
 - General testbench and trace tools
 ----------------------------------------------------------------------------
 -}


{-
 - Mealy machine that outputs its state as a Signal
 -
 - The feedback for the state is passed outside. This way, the caller can put
 - a trace on the state, e.g.:
 -
 - (s', o) = extractState tf is s'' i
 - s'' = traceSignal1 "s" s'
 -}
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

{-
 - Write a VCD file
 -
 - Variant of dumpVCD that writes its output to a file
 -}
writeVCD
    :: NFDataX a
    => FilePath
    -> (Int, Int)
    -> Signal dom a
    -> [String]
    -> IO ()
writeVCD fname slice signal traceNames = do
    vcd <- dumpVCD slice signal traceNames
    case vcd of
        Left s  -> error s
        Right d -> TIO.writeFile fname d
    putStrLn $ "Written file " P.++ fname

{-
 - Shorthand for writeVCD with a slice of (0,2000)
 -}
writeVCD'
    :: NFDataX a
    => FilePath
    -> Signal dom a
    -> [String]
    -> IO ()
writeVCD' = flip writeVCD (0,2000)

{-
 - Run a list of testbenches
 -
 - Samples each testbench in the list until it produces True.
 -}
runTBs
    :: KnownDomain dom
    => [Signal dom Bool]
    -> IO ()
runTBs tbs
    = if ( P.head $ dropWhile id
         $ P.map (P.head . dropWhile not . sample) tbs P.++ [False])
      then error "runTBs: impossible state"
      else putStrLn "Testbenches have run."

{-
 - Trace the elements of a two-tuple Signal individually
 -}
traceSignalT2
    :: ( KnownDomain dom
       , KnownNat (BitSize a)
       , KnownNat (BitSize b)
       , BitPack a
       , BitPack b
       , NFDataX a
       , NFDataX b
       , Typeable a
       , Typeable b
       )
    => (String, String)
    -> Signal dom (a, b)
    -> Signal dom (a, b)
traceSignalT2 (n1, n2) inp = bundle (s1', s2')
    where
        (s1,s2) = unbundle inp
        s1' = traceSignal1 n1 s1
        s2' = traceSignal1 n2 s2

{-
 - Lift seqX to applicative for easy infix use
 -}
seqXA :: Applicative f => f a -> f b -> f b
seqXA = liftA2 seqX
infixr 0 `seqXA`

{----------------------------------------------------------------------------
 - Dataflow-like simulation helpers
 ----------------------------------------------------------------------------
 -}


{-
 - stimuliGenerator with flow control
 -
 - Stimuli are generated as Maybe values and only consumed when ready is True.
 - At the end of the stimuli list, Nothing is output.
 -}
backPresStimuliGenerator
    :: ( KnownDomain dom
       , KnownNat n
       )
    => Clock dom
    -> Reset dom
    -> Vec n a
    -> Signal dom Bool
    -> Signal dom (Maybe a)
backPresStimuliGenerator clk rst stim ready
    = CEP.moore clk rst enableGen bpsgT (bpsgO stim) (0, False) ready
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

{-
 - outputVerifier' with flow control
 -
 - Verified output is in the form of Maybe. Just values are compared to a list
 - of expected samples, Nothings are discarded.
 -
 - Because a simulation of a system that produces fewer than the expected
 - number of samples would otherwise run forever, you can provide a `cutoff`
 - indicating the maximum number of clock cycles to run. If no cutoff is
 - desired, use 0.
 -}
maybeOutputVerifier'
    :: forall l cLim a dom
     . ( KnownDomain dom
       , KnownNat l
       , KnownNat cLim
       , Eq a
       , ShowX a
       )
    => Clock dom
    -> Reset dom
    -> "cutoff" ::: SNat cLim
    -> "samples" ::: Vec l a
    -> "circuitOutput" ::: Signal dom (Maybe a)
    -> "done" ::: Signal dom Bool
maybeOutputVerifier' =
    maybeOutputVerifier @l @cLim @a @dom @dom

maybeOutputVerifier
    :: forall l cLim a testDom circuitDom
     . ( KnownDomain testDom
       , KnownDomain circuitDom
       , KnownNat l
       , KnownNat cLim
       , Eq a
       , ShowX a
       )
    => Clock testDom
    -> Reset testDom
    -> "cutoff" ::: SNat cLim
    -> "samples" ::: Vec l a
    -> "circuitOutput" ::: Signal circuitDom (Maybe a)
    -> "done" ::: Signal testDom Bool
maybeOutputVerifier clk rst _ samples i0 =
        -- Only assert while not finished
        mux finish' finish' $ assertCLim
        $ CEP.assert clk rst "outputVerifier" i1 expect finish'
    where
        circuitPer = snatToNum (clockPeriod @circuitDom)
        testPer    = snatToNum (clockPeriod @testDom)
        i1         = CEP.veryUnsafeSynchronizer circuitPer testPer i0
        en         = toEnable (pure True)
        finish'    = CEP.register clk rst en False finish
        assertCLim
            | natToNatural @cLim == 0 = id
            | otherwise               =
                  CEP.assert clk rst "outputVerifier exceeded cycle limit"
                             cLimHit (pure False)
        (cLimHit, expect, finish) = unbundle $ CEP.mealy clk rst en genT (0,0)
                                                         i1

        genT :: (Index cLim, Index l)
             -> Maybe a
             -> ((Index cLim, Index l), (Bool, Maybe a, Bool))
        genT (ccnt, scnt) i = ((ccnt', scnt'), (cLimHit, expect, finish))
            where
                expect  = fmap (const $ samples !! scnt) i
                scnt'   = maybe scnt (const $ succBound (SNat @l) scnt) i
                cLimHit = not (natToNatural @cLim == 0) && ccnt == maxBound
                ccnt'   = succBound (SNat @cLim) ccnt
                finish  = scnt == maxBound || cLimHit

        succBound :: SNat n -> Index n -> Index n
        succBound sn@SNat =
            case compareSNat sn d0 of
                SNatLE -> const undefined
                SNatGT -> satSucc SatBound

{-
 - Stall a dataflow-like stream of Maybe
 -
 - Insert between a Maybe data stream and a ready signal. It will stall the
 - stream by making the ready signal False and the Maybe value Nothing at
 - given intervals of the simulation.
 -
 - Takes a vector (ss) of elements (takenum, stallnum) which means: take
 - takenum elements from the stream (Just values) and then stall for stallnum
 - cycles.
 -
 - Misbehaves for stallnum == 0, don't do that.
 -}
stallStream
    :: ( KnownDomain dom
       , KnownNat n
       )
    => Clock dom
    -> Reset dom
    -> Enable dom
    -> Vec n (Int, Int)
    -> Signal dom (Maybe a)
    -> ( Signal dom Bool
       , Signal dom (Maybe a)
       )

stallStream clk rst en ss sIn
    = unbundle $ CEP.mealy clk rst en (stallStreamT ss) (0,0,1) sIn

stallStreamT
    :: KnownNat n
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
