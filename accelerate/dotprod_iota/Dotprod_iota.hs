

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime) -- Not in 6.10
import System.CPUTime.Rdtsc (rdtsc)
import Control.Exception (evaluate)

import Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Cilk as Cilk
import qualified Data.Array.Accelerate.C    as C

dotprod :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotprod a b = A.fold (+) 0 (A.zipWith (*) a b)

iota :: Int -> Acc (Vector Float)
iota n = A.generate (index1$ constant n) (A.fromIntegral . unindex1)

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

