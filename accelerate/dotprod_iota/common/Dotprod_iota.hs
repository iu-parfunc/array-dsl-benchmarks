{-# LANGUAGE CPP, NamedFieldPuns #-}

module Main where


import Data.Array.Accelerate as Acc hiding ((++))
import Prelude               as P

import Data.Array.Accelerate as A
#ifdef ACCBACKEND
import qualified ACCBACKEND as Bkend
#else
#error "Must specify ACCBACKEND CPP variable to build this nbody benchmark."
-- import qualified Data.Array.Accelerate.CUDA as Bkend
#endif

import           Data.Array.Accelerate.BackendClass (runTimed, AccTiming(..))
import           Data.Time.Clock (getCurrentTime, diffUTCTime)

import Control.Exception (evaluate)
import System.Environment (getArgs)


dotprod :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotprod a b = A.fold (+) 0 (A.zipWith (*) a b)

iota :: Int -> Acc (Vector Float)
iota n = A.generate (index1$ constant n) (A.fromIntegral . unindex1)

main = do args <- getArgs
          let inputSize = case args of
                []   -> 1000000
                [sq] -> read sq
          putStrLn$ "Input Size: "P.++ show inputSize
          let arr = iota inputSize
          tBegin <- getCurrentTime
          (times, _output) <- runTimed Bkend.defaultBackend Nothing Bkend.defaultTrafoConfig (dotprod arr arr)
          tEnd <- getCurrentTime
          let AccTiming{compileTime,runTime,copyTime} = times
          putStrLn$ "  All timing: "P.++ show times
          putStrLn$ "  Total time for runTimed "P.++ show (diffUTCTime tEnd tBegin)
#ifndef DONTPRINT       
          putStrLn$ "JITTIME: "P.++ show compileTime
          putStrLn$ "SELFTIMED: "P.++ show (runTime + copyTime)
#endif
              

{-
main = do
  t1 <- getCurrentTime
  c1 <- rdtsc
  let arr = iota 1000000
--  x <- evaluate$ Cilk.run (dotprod arr arr)
  x <- evaluate$ C.run (dotprod arr arr)      
  c2 <- rdtsc
  t2 <- getCurrentTime
  let secs = diffUTCTime t2 t1
      cyc  = c2 - c1
  putStrLn$ "Time to compile&run: "++show secs++" seconds, "++show cyc++" cycles."

-}
