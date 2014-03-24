
-- | A simple reduction microbenchmark.

module Main where

import System.Random.MWC
import Data.Array.IArray     as IArray
import Data.Array.Accelerate as Acc
import Prelude               as P

#ifdef ACCBACKEND
import qualified ACCBACKEND as Bkend
#else
#error "Must specify ACCBACKEND CPP variable to build this benchmark."
-- import qualified Data.Array.Accelerate.CUDA as Bkend
#endif

import Data.Array.Accelerate as A 
import Data.Array.Accelerate as Acc
-- import Data.Array.Accelerate.BackendClass (runTimed, AccTiming(..), Backend(..))
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Control.Exception (evaluate)
import System.Environment (getArgs)
--------------------------------------------------------------------------------

{-
-- SAXPY
-- -----
foldem :: Vector Float -> Acc (Scalar Float)
foldem alpha xs ys
  = let
      xs' = use xs
      ys' = use ys
    in
    Acc.zipWith (\x y -> constant alpha * x + y) xs' ys'

saxpyRef :: Float -> UArray Int Float -> UArray Int Float -> UArray Int Float
saxpyRef alpha xs ys
  = listArray (bounds xs) [alpha * x + y | x <- elems xs | y <- elems ys]

-- Main
-- ----

runrun :: Int -> IO (() -> UArray Int Float, () -> Acc (Vector Float))
runrun nelements = withSystemRandom $ \gen -> do
  v1    <- randomUArrayR (-1,1) gen nelements
  v2    <- randomUArrayR (-1,1) gen nelements
  v1'   <- convertUArray v1
  v2'   <- convertUArray v2
  alpha <- uniform gen
  --
  return (run_ref alpha v1 v2, run_acc alpha v1' v2')
  where
    {-# NOINLINE run_ref #-}
    run_ref alpha xs ys () = saxpyRef alpha xs ys
    run_acc alpha xs ys () = saxpyAcc alpha xs ys
-}


--  
reduce :: Acc (Vector Float) -> Acc (Scalar Float)
reduce arr = A.fold (+) 0 arr


main :: IO ()
main = do args <- getArgs 
          let inputSize = case args of
                            []   -> 100
                            [sz] -> read sz
--           (_,go) <- (runrun inputSize)

          tBegin <- getCurrentTime
{-
          (times,_output) <- runTimed Bkend.defaultBackend Nothing Bkend.defaultTrafoConfig (go ())
          tEnd   <- getCurrentTime
          let AccTiming{compileTime,runTime,copyTime} = times
          putStrLn$ "  All timing: "++ show times
          putStrLn$ "  Total time for runTimed "++ show (diffUTCTime tEnd tBegin)
          putStrLn$ "JITTIME: "++ show compileTime
          putStrLn$ "SELFTIMED: "++ show (runTime + copyTime)
-}
          return ()
