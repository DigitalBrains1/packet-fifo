{-
 - Copyright (c) 2019,2020 QBayLogic B.V.
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
    = print $ P.head $ dropWhile id
    $ P.map (P.head . dropWhile not . sample) tbs P.++ [False]

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
 -}
maybeOutputVerifier'
    :: ( KnownDomain dom
       , KnownNat n
       , Eq a
       , ShowX a
       )
    => Clock dom
    -> Reset dom
    -> Vec n a
    -> Signal dom (Maybe a)
    -> Signal dom Bool
maybeOutputVerifier' clk rst samples
    = CEP.moore clk rst enableGen (movT samples) movO (0, False)
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
