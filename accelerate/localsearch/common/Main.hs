{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedStrings #-}
--
-- A simple implementation of parallel bit climbing.
--

module Main where

import           Data.Array.Accelerate          as A hiding ((++))
import           Data.Array.Accelerate          ((:.),Z(Z))
#ifdef ACCBACKEND
import qualified ACCBACKEND as Bkend
#else
#error "Must specify ACCBACKEND CPP variable to build this nbody benchmark."
-- import qualified Data.Array.Accelerate.CUDA as Bkend
#endif
import qualified Data.Array.Accelerate.Interpreter as I
import           Data.Array.Accelerate.BackendClass 
import Data.Array.Accelerate.BackendKit.CompilerPipeline (phase0, phase1)

-- system
import Control.Exception (evaluate)
import Control.Monad     (forM_,when, replicateM)
import Prelude                                  as P
import           Data.Char (isSpace)
import qualified Data.Array.Unboxed             as U
import qualified Data.List as L
import           Data.Time.Clock (getCurrentTime, diffUTCTime)
-- import qualified Data.Array.IO                  as Arr
-- import qualified Data.ByteString.Lazy.Char8 as B
import System.Environment
import System.Random
import System.IO
import System.Environment (getEnvironment)
import System.Mem  (performGC)
import Data.Maybe (fromJust)
import Debug.Trace 

#ifdef EXTRAINITCUDA
import Foreign.CUDA.Driver (initialise)
#endif

--------------------------------------------------------------------------------
-- Parmeters
--------------------------------------------------------------------------------

default_bitstringSize = 20
default_bitstringNum  = 1000
-- default_searchIters   = 100
default_searchIters   = 10
numRangeLow   = -20
numRangeHigh  = 20

--------------------------------------------------------------------------------
-- MAIN script
--------------------------------------------------------------------------------

main :: IO ()
main = do
#ifdef EXTRAINITCUDA
  initialise []
  putStrLn$"CUDA initialized - this is a hack to work around an apparent accelerate-cuda bug."
#endif

  args <- getArgs
  let [searchIters, bitstringNum, bitstringSize] = 
        P.map read args ++ 
        P.take (3 - length args) [default_searchIters, default_bitstringNum, default_bitstringSize]
  putStrLn$ "Got "++show (length args)++" command line args."
  putStrLn$ " Params (searchIters, bitstringNum, bitstringSize) = "++show (searchIters, bitstringNum, bitstringSize)

  tBegin <- getCurrentTime
  performGC
  tEnd   <- getCurrentTime

  putStrLn$ "Input in CPU memory and did GC (took "++show (diffUTCTime tEnd tBegin)++"), starting benchmark..."
  tBegin <- getCurrentTime
  g <- newStdGen
  strs <- makeAllStrings bitstringNum bitstringSize
  let arrs     = fromList (Z:.bitstringNum:.bitstringSize) (concat strs)
      useArrs  = use arrs
      passNum  = unit . constant
      evalFunc = evaluateNum -- pass number parameters as unit arrays
                 (passNum 0) (passNum (2^bitstringSize - 1))
                 (passNum numRangeLow) (passNum numRangeHigh)
      rns      = P.take searchIters $ randomRs (0, bitstringSize-1) g

  let fullacc = evalFunc $ translateNums $ runSearch rns evalFunc useArrs
      simpl = phase1 $ phase0 fullacc
#ifndef NOSIMPLE
  (times, output) <- runTimedSimple Bkend.defaultBackend Nothing Bkend.defaultTrafoConfig simpl
  tEnd   <- getCurrentTime
  putStrLn$ "Finished executing through SimpleBackend. "
#else
  (times, output) <- runTimed Bkend.defaultBackend Nothing Bkend.defaultTrafoConfig fullacc
  tEnd   <- getCurrentTime
  putStrLn$ "  Result: "++ show(P.minimum $ A.toList output)
#endif
  let AccTiming{compileTime,runTime,copyTime} = times
  putStrLn$ "  All timing: "++ show times
  putStrLn$ "  Total time for runTimed "++ show (diffUTCTime tEnd tBegin)
  putStrLn$ "JITTIME: "++ show compileTime
  putStrLn$ "SELFTIMED: "++ show (runTime + copyTime)


----------------------------------------------------------------------------------------------------
-- The actual benchmark code:
----------------------------------------------------------------------------------------------------

-- I got tired of typing these so I made shortcuts
type TwoD     = Array DIM2 Int
type OneD     = Array DIM1 Int
type OneDF    = Array DIM1 Float
type EvalFunc = Acc OneD -> Acc OneDF

-- Convert arrays of 1s and 0s into integers.
translateNums :: Acc TwoD -> Acc OneD
translateNums = process
  where process     = powers >-> sum
        f strs ix   = let i = indexHead ix
                          n = strs ! ix
                      in shiftL n i
        powers strs = generate (shape strs) (f strs)
        sum         = A.fold1 (+)

-- Generate a new list of neighboring numbers
makeNeighbors :: Acc (Scalar Int) -> Acc TwoD -> Acc TwoD
makeNeighbors place strs = flip
  where place' = the place
        len    = indexHead (shape strs)
        f ix   = let Z :. j :. i = unlift ix
                     modEq       = (i `mod` len) ==* place'
                     value       = strs ! index2 j i
                     result      = complementBit value 0
                 in modEq ? (result, value)
        flip   = generate (shape strs) f                   

-- Pick new neighbors that are better (minimizing eval score).
compareNeighbors :: EvalFunc -> Acc TwoD -> Acc TwoD -> Acc TwoD
compareNeighbors evalFunc str1 str2 = finalStr
  where process  = translateNums >-> evalFunc
        str1e    = process str1
        str2e    = process str2
        f ix     = let Z :. j :. i = unlift ix
                       e1          = str1e ! index1 j
                       e2          = str2e ! index1 j
                       v1          = str1 ! index2 j i
                       v2          = str2 ! index2 j i
                   in e1 <* e2 ? (v1, v2)
        finalStr = generate (shape str1) f                     

-- Pass numbers to sinbowl function.
evaluateNum :: Acc (Scalar Float) -> Acc (Scalar Float) ->
               Acc (Scalar Float) -> Acc (Scalar Float) ->
               Acc OneD -> Acc OneDF
evaluateNum min max a b = process
  where process = A.map scale >-> A.map f
        min'    = the min
        max'    = the max
        a'      = the a
        b'      = the b
        scale x = let x' = A.fromIntegral x
                  in (((b' - a') * (x' - min')) / (max' - min')) + a'
        f x     = abs x * 0.1 - sin x

-- Random numbers are awkward in Haskell
makeRandomString :: Int -> IO [Int]
makeRandomString size = do
  g <- newStdGen
  return $ P.take size (randomRs (0,1) g :: [Int])

-- Use makeRandomString to generate all the random strings.
makeAllStrings :: Int -> Int -> IO [[Int]]
makeAllStrings num size = replicateM num $ makeRandomString size

-- Given a list of random numbers, an evaluation function, and an
-- initial population, run the search using foldr. The number of
-- iterations will be the length of rn.
runSearch :: [Int] -> EvalFunc -> Acc TwoD -> Acc TwoD
runSearch rn f strs = foldr compute strs rn
  where compute r strs = let flipBit   = unit $ constant r
                             neighbors = makeNeighbors flipBit strs
                         in compareNeighbors f strs neighbors

