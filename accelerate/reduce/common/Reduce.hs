{-# LANGUAGE CPP, NamedFieldPuns #-}

-- | A simple reduction microbenchmark.

module Main where
import Control.Monad
import Control.Exception
import System.Random.MWC
import Data.Array.IArray     as IArray
import Prelude               as P
import RandomArray (randomUArrayR, convertUArray)

#ifdef ACCBACKEND
import qualified ACCBACKEND as Bkend
#else
#error "Must specify ACCBACKEND CPP variable to build this benchmark."
-- import qualified Data.Array.Accelerate.CUDA as Bkend
#endif

import Data.Array.Accelerate  as A  hiding ((++))
-- import Data.Array.Accelerate.BackendClass (runTimed, AccTiming(..), Backend(..))
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Control.Exception (evaluate)
import System.Environment (getArgs)
--------------------------------------------------------------------------------


-- runrun :: Int -> IO (() -> UArray Int Float, () -> Acc (Vector Float))
-- mkArr :: Int -> IO (UArray Int Float)
mkArr :: Int -> IO (Vector Float)
-- mkArr nelements = withSystemRandom $ \gen -> do
--   v1  <- randomUArrayR (-1,1) gen nelements
--   v1' <- convertUArray v1
--   return v1'
mkArr nelements = return $! 
    Bkend.run (A.generate (index1 (constant nelements)) (A.fromIntegral . unindex1))

reduce :: Acc (Vector Float) -> Acc (Scalar Float)
reduce arr = A.fold (+) 0 arr

dummy :: Acc (Vector Float) -> Acc (Scalar Float)
dummy arr = A.unit $ arr A.! (index1 0)

main :: IO ()
main = do args <- getArgs 
          let inputSize = case args of
                            []   -> 100
                            [sz] -> read sz
          
          putStrLn$ "Running with array size "++ show inputSize
          (t0,input) <- timeit$ mkArr inputSize
          putStrLn$ "Time to create input array: "++ show t0

          let inp' = A.use input 
              go0  = dummy inp'
              go   = reduce inp'              

          (copy, _) <- timeit$ evaluate $ Bkend.run go0
          -- (copy, (_,_output)) <- timeit$ runTimed Bkend.defaultBackend Nothing Bkend.defaultTrafoConfig go0
          putStrLn$ "Time to copy and do the dummy computation: "++ show copy

          let loop 0 res   = return $! res
              loop n (j,t) = do 
                (times,_) <- timeit $ evaluate $ Bkend.run go
                                   -- runTimed Bkend.defaultBackend Nothing Bkend.defaultTrafoConfig go
--                let AccTiming{compileTime,runTime,copyTime} = times
--                loop (n-1) (j+compileTime, t+runTime+copyTime)
--                loop (n-1) (j+compileTime, t+runTime)
                loop (n-1) (0, t+times)
          tBegin <- getCurrentTime
          -- Run the kernel 1000 times:                               
          (jittime,runtime) <- loop 1000 (0,0)
          tEnd   <- getCurrentTime
          putStrLn$ "  Total time for 1000 kernels "++  show (diffUTCTime tEnd tBegin)
          putStrLn$ "  Summed times for EACH kernel "++ show runtime
          putStrLn$ "JITTIME: "  ++ show jittime
          putStrLn$ "SELFTIMED: "++ show runtime
          putStrLn "Done with benchmark."
          return ()

-- timeit :: Fractional t => IO a -> (a,t)
timeit :: IO a -> IO (Double,a)
timeit act = do 
   tBegin <- getCurrentTime
   x <- act
   tEnd   <- getCurrentTime
   return $! (fromRational $ toRational $ diffUTCTime tEnd tBegin, x)
