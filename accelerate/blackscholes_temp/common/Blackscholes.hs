{-# LANGUAGE CPP, NamedFieldPuns #-}
module Main where

import Random (randomUArrayR)

import System.Random.MWC
import Data.Array.IArray     as IArray
import Data.Array.Accelerate as A
import Prelude               as P

-- #ifdef ACCBACKEND
-- import qualified ACCBACKEND as Bkend
-- #else
-- #error "Must specify ACCBACKEND CPP variable to build this nbody benchmark."
-- -- import qualified Data.Array.Accelerate.CUDA as Bkend
-- #endif

import Data.Array.Accelerate.Trafo (convertAccWith, Phase(..))
import qualified Data.Array.Accelerate.BackendClass as BC
import           Data.Array.Accelerate.BackendClass (runTimed, AccTiming(..))
import           Data.Time.Clock (getCurrentTime, diffUTCTime)

import Control.Exception (evaluate)
import System.Environment (getArgs)
import System.Exit 
import Data.Array.Accelerate.Multi (runMultiple)
import qualified Data.Array.Accelerate.CUDA as CUDA
import qualified Data.Array.Accelerate.Cilk as Cilk


riskfree, volatility :: Float
riskfree   = 0.02
volatility = 0.30

-- Black-Scholes option pricing
-------------------------------

horner :: Num a => [a] -> a -> a
horner coeff x = x * foldr1 madd coeff
  where
    madd a b = a + x*b

cnd' :: Floating a => a -> a
cnd' d =
  let poly     = horner coeff
      coeff    = [0.31938153,-0.356563782,1.781477937,-1.821255978,1.330274429]
      rsqrt2pi = 0.39894228040143267793994605993438
      k        = 1.0 / (1.0 + 0.2316419 * abs d)
  in
  rsqrt2pi * exp (-0.5*d*d) * poly k


blackscholesAcc :: Vector (Float, Float, Float) -> Acc (Vector (Float, Float))
blackscholesAcc xs = A.map go (A.use xs)
  where
  go x =
    let (price, strike, years) = A.unlift x
        r       = A.constant riskfree
        v       = A.constant volatility
        v_sqrtT = v * sqrt years
        d1      = (log (price / strike) + (r + 0.5 * v * v) * years) / v_sqrtT
        d2      = d1 - v_sqrtT
        cnd d   = let c = cnd' d in d >* 0 ? (1.0 - c, c)
        cndD1   = cnd d1
        cndD2   = cnd d2
        x_expRT = strike * exp (-r * years)
    in
    A.lift ( price * cndD1 - x_expRT * cndD2
             , x_expRT * (1.0 - cndD2) - price * (1.0 - cndD1))


blackscholesRef :: IArray.Array Int (Float,Float,Float) -> IArray.Array Int (Float,Float)
blackscholesRef xs = listArray (bounds xs) [ go x | x <- elems xs ]
  where
  go (price, strike, years) =
    let r     = riskfree
        v     = volatility
        sqrtT = sqrt years
        d1    = (log (price / strike) + (r + 0.5 * v * v) * years) / (v * sqrtT)
        d2    = d1 - v * sqrtT
        cnd d = if d > 0 then 1.0 - cnd' d else cnd' d
        cndD1 = cnd d1
        cndD2 = cnd d2
        expRT = exp (-r * years)
    in
    ( price * cndD1 - strike * expRT * cndD2
    , strike * expRT * (1.0 - cndD2) - price * (1.0 - cndD1))


-- Main
-- ----

type Ty = (A.Vector (Float,Float))

run :: Int -> IO (() -> IArray.Array Int (Float,Float), () -> Acc (Vector (Float,Float)))
run n = withSystemRandom $ \gen -> do
  v_sp <- randomUArrayR (5,30)    gen n
  v_os <- randomUArrayR (1,100)   gen n
  v_oy <- randomUArrayR (0.25,10) gen n

  let v_psy = listArray (0,n-1) $ P.zip3 (elems v_sp) (elems v_os) (elems v_oy)
      a_psy = A.fromIArray v_psy
  --
  return (run_ref v_psy, run_acc a_psy)
  where
    {-# NOINLINE run_ref #-}
    run_ref psy () = blackscholesRef psy
    run_acc psy () = blackscholesAcc psy


main = do args <- getArgs 
          let inputSize = case args of
                            [] -> 100
                            [sz] -> read sz
          let half  = (inputSize `quot` 2)
          (_,run_acc) <- run half

          let prog1 = run_acc ()
              prog2 = prog2

          let x :: (Acc Ty, BC.SomeBackend, Phase)
              x = ( prog1, BC.SomeBackend CUDA.defaultBackend, CUDA.defaultTrafoConfig) 

          let y :: (Acc Ty, BC.SomeBackend, Phase)
              y = ( prog2, BC.SomeBackend Cilk.defaultBackend, Cilk.defaultTrafoConfig)

          -- (time,_) <- timeit$ runMultiple [ y ]
          runMultiple [ x,y ]
          putStrLn "All done with runMultiple!"

          -- (times,_output) <- runTimed Cilk.defaultBackend Nothing Cilk.defaultTrafoConfig prog1
          -- putStrLn$ "  All timing: "++ show times
          exitSuccess

          -- tBegin <- getCurrentTime
          -- (times,_output) <- runTimed Bkend.defaultBackend Nothing Bkend.defaultTrafoConfig (run_acc ())
          -- tEnd   <- getCurrentTime
          -- let AccTiming{compileTime,runTime,copyTime} = times

          -- putStrLn$ "  Total time for runTimed "++ show (diffUTCTime tEnd tBegin)
          -- putStrLn$ "JITTIME: "++ show compileTime
          -- putStrLn$ "SELFTIMED: "++ show (runTime + copyTime)

