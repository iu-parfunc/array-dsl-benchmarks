{-# LANGUAGE CPP, NamedFieldPuns #-}

module Main where

#ifdef ACCBACKEND
import qualified ACCBACKEND as Bkend
#else
#error "Must specify ACCBACKEND CPP variable to build this nbody benchmark."
#endif

import Data.Time.Clock (getCurrentTime, diffUTCTime) -- Not in 6.10
-- import System.CPUTime.Rdtsc (rdtsc)
import Control.Exception (evaluate)
import Data.Array.Accelerate as A
import Data.Array.Accelerate.BackendClass (runTimed, AccTiming(..))
import System.Environment
import System.Mem (performGC)
import Prelude as P
import RandomArray (convertVector, randomVectorR)
import System.Random.MWC(createSystemRandom)    
import qualified Data.Vector.Unboxed as U

--------------------------------------------------------------------------------


-- | Stack up N sqrts in a row.
sqrts :: Int -> Acc (Vector Float) -> Acc (Vector Float)
sqrts n = 
   A.map (nscalarSqrts n)
   -- A.map (scalePows n)
  where
   nscalarSqrts 0 exp = exp
   nscalarSqrts n exp = P.sqrt (3.33 * (nscalarSqrts (n-1) exp))
   
   scalePows n x = let n' = A.fromIntegral $ A.constant n in 
                   (1.001 ** n') * (0.999 ** n') * x 
     

main = do
  args <- getArgs
  let (num,size) = case args of 
                      []    -> (1,100)
                      [n]   -> (read n,100)
                      [n,s] -> (read n, read s)
  putStrLn$ "[*] Running flops/byte scaling test, array size "++show size++", num sqrts "++show num
  tBegin <- getCurrentTime
  genio  <- createSystemRandom
  vec    <- randomVectorR (10.0, 100.0) genio size
  array  <- convertVector (vec :: U.Vector Float)
  evaluate array
  performGC
  tEnd   <- getCurrentTime
  putStrLn$ "Random Input generated in CPU memory (took "++show (diffUTCTime tEnd tBegin)++"), starting benchmark..."
  
  tBegin <- getCurrentTime
  (times,output) <- runTimed Bkend.defaultBackend Nothing Bkend.defaultTrafoConfig (sqrts num (A.use array))
  tEnd   <- getCurrentTime
  let AccTiming{compileTime,runTime,copyTime} = times
  putStrLn$ "  Result prefix(4): "++ show(P.take 3$ A.toList output)
  putStrLn$ "  Result shape "++ show(A.arrayShape output)
  putStrLn$ "  All timing: "++ show times
  putStrLn$ "  Total time for runTimed "++ show (diffUTCTime tEnd tBegin)
  putStrLn$ "JITTIME: "++ show compileTime
  putStrLn$ "SELFTIMED: "++ show (runTime + copyTime)

