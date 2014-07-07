{-# LANGUAGE CPP, NamedFieldPuns #-}
module Main where

import Random (randomUArrayR)

import System.Random.MWC
import Data.Array.IArray     as IArray
import Data.Array.Accelerate as Acc hiding ((++))
import Prelude               as P

#ifdef ACCBACKEND
import qualified ACCBACKEND as Bkend
#else
#error "Must specify ACCBACKEND CPP variable to build this nbody benchmark."
-- import qualified Data.Array.Accelerate.CUDA as Bkend
#endif

import Control.Exception (evaluate)
import Data.Array.Accelerate.BackendClass (runTimed, AccTiming(..), SimpleBackend(..))
import Data.Array.Accelerate.BackendKit.CompilerPipeline (phase0, phase1)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
-- import Foreign.CUDA.Driver (initialise)
import System.Environment (getArgs)

riskfree, volatility :: Float
topLoop :: Int
riskfree   = 0.02
volatility = 0.30
topLoop    = 1000 -- how many pointless iterations to do

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


blackscholesAcc :: Acc (Acc.Array DIM2 (Float, Float, Float)) -> 
                   Acc (Acc.Array DIM2 (Float, Float))
blackscholesAcc xs = mat
  where mat = Acc.map go xs
        go x = let price,strike,years :: Exp Float
                   (price,strike,years) = Acc.unlift x
                   r       = Acc.constant riskfree
                   v       = Acc.constant volatility
                   v_sqrtT = v * sqrt years
                   d1      = (log (price / strike) + (r + 0.5 * v * v) * years) / v_sqrtT
                   d2      = d1 - v_sqrtT
                   cnd d   = let c = cnd' d in d >* 0 ? (1.0 - c, c)
                   cndD1   = cnd d1
                   cndD2   = cnd d2
                   x_expRT = strike * exp (-r * years)
              in Acc.lift ( price * cndD1 - x_expRT * cndD2
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

run :: Int -> IO (() -> IArray.Array Int (Float,Float), () -> Acc (Acc.Array DIM2 (Float,Float)))
run n = withSystemRandom $ \gen -> do
  v_sp <- randomUArrayR (5,30)    gen n
  v_os <- randomUArrayR (1,100)   gen n
  v_oy <- randomUArrayR (0.25,10) gen n

  let v_psy = listArray (0,n-1) $ P.zip3 (elems v_sp) (elems v_os) (elems v_oy)
      a_psy = Acc.use $ Acc.fromIArray v_psy :: Acc (Acc.Array DIM1 (Float, Float, Float))
      r_psy = Acc.replicate (constant (Z :. topLoop :. All)) a_psy :: Acc (Acc.Array DIM2 (Float, Float, Float))
  --
  return (run_ref v_psy, run_acc r_psy)
  where
    {-# NOINLINE run_ref #-}
    run_ref psy () = blackscholesRef psy
    run_acc psy () = blackscholesAcc psy


main = do args <- getArgs 
          let inputSize = case args of
                            [] -> 100
                            [sz] -> read sz
          putStrLn$"Blackscholes running on input size: "++show inputSize
          (_,run_acc) <- run inputSize -- 0000
          -- initialise []
          -- putStrLn$"CUDA initialized."

          let simpl = phase1 $ phase0 $ run_acc ()
          tBegin <- getCurrentTime
#ifndef NOSIMPLE
          rmts <- simpleRunRaw Bkend.defaultBackend Nothing simpl Nothing
          -- mapM_ simpleWaitRemote rmts
          mapM_ (simpleCopyToHost Bkend.defaultBackend) rmts
          tEnd   <- getCurrentTime
          putStrLn$ "Finished executing through SimpleBackend. "
#else
          (times,_output) <- runTimed Bkend.defaultBackend Nothing Bkend.defaultTrafoConfig (run_acc ())
          let AccTiming{compileTime,runTime,copyTime} = times
          putStrLn$ "  All timing: "P.++ show times
#  ifndef DONTPRINT       
          putStrLn$ "JITTIME: "P.++ show compileTime
          putStrLn$ "SELFTIMED: "P.++ show (runTime + copyTime)
#  endif
          tEnd   <- getCurrentTime
#endif


          putStrLn$ "  Total time: "P.++ show (diffUTCTime tEnd tBegin)

          -- let vec = Bkend.run $ run_acc ()

          -- t1 <- getCurrentTime
          -- evaluate vec
          -- t2 <- getCurrentTime
          -- let dt = t2 `diffUTCTime` t1
          -- putStrLn$ "SELFTIMED-with-compile: "++ show dt
--	  putStrLn$ "Final checksum: "++ show sum
  
