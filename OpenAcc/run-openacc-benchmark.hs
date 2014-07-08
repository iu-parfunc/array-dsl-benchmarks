{-# LANGUAGE NamedFieldPuns #-}

-- | HSBencher script to run all the benchmarks.

module Main where

import HSBencher
import HSBencher.Methods.Builtin (makeMethod)
import HSBencher.Backend.Fusion  (defaultFusionPlugin)
import HSBencher.Backend.Dribble (defaultDribblePlugin)

--------------------------------------------------------------------------------

benches :: [Benchmark DefaultParamMeaning]
benches = 
  [ mkBenchmark "./nbody/" [show x] def | x <- nbodySizes, def <- defaults ]
--  , mkBenchmark "./blackscholes/" [] defaults

nbodySizes :: [Integer]
nbodySizes = [10000, 20000, 30000,
              40000, 50000, 80000,
              100000, 160000]

defaults = [And [ Set (Variant "OpenAcc") (RuntimeEnv "PGI_ACC_TIME" "1")
               , Set NoMeaning           (RuntimeEnv "ACC_NOTIFY"   "1") ],
            And [ Set (Variant "OpenMP") (RuntimeEnv "PGI_ACC_TIME" "1")
                , Set NoMeaning           (RuntimeEnv "ACC_NOTIFY"   "1")
                  Set NoMeaning           (RuntimeEnv "PROG_SUFFIX"  "-omp")]]
        
main :: IO ()
main = do
  putStrLn "Begin Racket type-checking benchmarks..."
  -- putStrLn$ "Automatic thread selection: "++show threadSelection
  defaultMainModifyConfig $ \ conf ->
    conf{ benchlist  = benches
        , runTimeOut = Just 1000 -- Erk... need a separate compile timeout.
        , buildMethods = [makeMethod]
        , plugIns   = [ SomePlugin defaultFusionPlugin,
                        SomePlugin defaultDribblePlugin ]
        }
