{-# LANGUAGE CPP, NamedFieldPuns #-}

-- | A simple reduction microbenchmark.

module Main where
import Control.Monad
import Control.Exception
import System.Random.MWC
import Data.Array.IArray as IArray hiding ((!))
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

reduceLoop :: Int -> Acc (Vector Float) -> Acc (Scalar Float)
reduceLoop iters arr = 
     let a :: Acc (Scalar Int)
         b :: Acc (Scalar Float)
         c :: Acc (Vector Float)
         (a,b,c) = unlift final
     in b
  where 
  final :: Acc (Scalar Int, Scalar Float, Vector Float)
  final = A.awhile test body start

  start :: Acc (Scalar Int, Scalar Float, Vector Float)
  start = lift (A.unit 0, A.unit 0, arr)

  test :: Acc (Scalar Int, Scalar Float, Vector Float) -> Acc (Scalar Bool)
  test arr = 
     let a :: Acc (Scalar Int)
         b :: Acc (Scalar Float)
         c :: Acc (Vector Float)
         (a,b,c) = unlift arr 
     in A.unit $   a ! index0 ==* constant iters
               ||* b ! index0 ==* 99 -- Use this for something so it doesn't get eliminated

  body :: Acc (Scalar Int, Scalar Float, Vector Float) ->
          Acc (Scalar Int, Scalar Float, Vector Float)
  body arr = 
     let a :: Acc (Scalar Int)
         b :: Acc (Scalar Float)
         c :: Acc (Vector Float)
         (a,b,c) = unlift arr 
     in lift (a,reduce c,c)

main :: IO ()
main = do args <- getArgs 
          let (inputSize,mode) = case args of
                                  []      -> (100,"awhile")
                                  [sz]    -> (read sz,"awhile")
                                  [sz,md] -> (read sz,md)

          putStrLn$ "Running with array size "++ show inputSize ++" mode: " ++mode
          (t0,input) <- timeit$ mkArr inputSize
          putStrLn$ "Time to create input array: "++ show t0

          let inp' = A.use input 
              go0  = dummy inp'
              goLoop   = reduce inp' 
              goAwhile = reduceLoop totaliters inp'

              gogo = case mode of
                      "awhile" -> goAwhile
                      "loop"   -> goLoop
                      oth      -> error$"Reduce.hs, unrecognized mode as second command line argument: "++mode

          (copy, _) <- timeit$ evaluate $ Bkend.run go0
          -- (copy, (_,_output)) <- timeit$ runTimed Bkend.defaultBackend Nothing Bkend.defaultTrafoConfig go0
          putStrLn$ "Time to copy and do the dummy computation: "++ show copy

          let loop 0 res   = return $! res
              loop n (j,t) = do 
                (times,_) <- timeit $ evaluate $ Bkend.run gogo
                                   -- runTimed Bkend.defaultBackend Nothing Bkend.defaultTrafoConfig go
--                let AccTiming{compileTime,runTime,copyTime} = times
--                loop (n-1) (j+compileTime, t+runTime+copyTime)
--                loop (n-1) (j+compileTime, t+runTime)
                loop (n-1) (0, t+times)
          tBegin <- getCurrentTime
          -- Run the kernel 1000 times:                               
          (jittime,runtime) <- loop totaliters (0,0)
          tEnd   <- getCurrentTime
          putStrLn$ "  Total time for 1000 kernels "++  show (diffUTCTime tEnd tBegin)
          putStrLn$ "  Summed times for EACH kernel "++ show runtime
          putStrLn$ "JITTIME: "  ++ show jittime
          putStrLn$ "SELFTIMED: "++ show runtime
          putStrLn "Done with benchmark."
          return ()

totaliters = 1000

-- timeit :: Fractional t => IO a -> (a,t)
timeit :: IO a -> IO (Double,a)
timeit act = do 
   tBegin <- getCurrentTime
   x <- act
   tEnd   <- getCurrentTime
   return $! (fromRational $ toRational $ diffUTCTime tEnd tBegin, x)
