{-# LANGUAGE CPP, NamedFieldPuns #-}

-- | A simple reduction microbenchmark.

module Main where

import System.Random.MWC
import Data.Array.IArray     as IArray
import Data.Array.Accelerate as Acc
import Prelude               as P
import Random (randomUArrayR, convertUArray)

#ifdef ACCBACKEND
import qualified ACCBACKEND as Bkend
#else
#error "Must specify ACCBACKEND CPP variable to build this benchmark."
-- import qualified Data.Array.Accelerate.CUDA as Bkend
#endif

import Data.Array.Accelerate as A 
import Data.Array.Accelerate as Acc
import Data.Array.Accelerate.BackendClass (runTimed, AccTiming(..), Backend(..))
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Control.Exception (evaluate)
import System.Environment (getArgs)
--------------------------------------------------------------------------------


-- runrun :: Int -> IO (() -> UArray Int Float, () -> Acc (Vector Float))
-- mkArr :: Int -> IO (UArray Int Float)
mkArr :: Int -> IO (Vector Float)
mkArr nelements = withSystemRandom $ \gen -> do
  v1    <- randomUArrayR (-1,1) gen nelements
  v1'   <- convertUArray v1
  return v1'
--  alpha <- uniform gen
--  run_acc alpha xs ys () = saxpyAcc alpha xs ys

--  
reduce :: Acc (Vector Float) -> Acc (Scalar Float)
reduce arr = A.fold (+) 0 arr


main :: IO ()
main = do args <- getArgs 
          let inputSize = case args of
                            []   -> 100
                            [sz] -> read sz
          
          (t0,input) <- timeit$ mkArr inputSize

          let go = reduce (A.use input)

          tBegin <- getCurrentTime
          (times,_output) <- runTimed Bkend.defaultBackend Nothing Bkend.defaultTrafoConfig go
          tEnd   <- getCurrentTime
          let AccTiming{compileTime,runTime,copyTime} = times
          putStrLn$ "  All timing: "++ show times
          putStrLn$ "  Total time for runTimed "++ show (diffUTCTime tEnd tBegin)
          putStrLn$ "JITTIME: "++ show compileTime
          putStrLn$ "SELFTIMED: "++ show (runTime + copyTime)

          putStrLn "Done with benchmark."
          return ()

-- timeit :: Fractional t => IO a -> (a,t)
timeit :: IO a -> IO (Double,a)
timeit act = do 
   tBegin <- getCurrentTime
   x <- act
   tEnd   <- getCurrentTime
   return $! (fromRational $ toRational $ diffUTCTime tEnd tBegin, x)
