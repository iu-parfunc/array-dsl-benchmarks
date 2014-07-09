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
  [ (mkBenchmark "./nbody/" [show x] openAccOpts)
    { progname = Just "openacc-nbody" } | x <- nbodySizes ]
  
--  [ (mkBenchmark "./nbody/" [show x] openMPOpts)  { progname = Just "openmp-nbody"  } | x <- nbodySizes ]
--  , mkBenchmark "./blackscholes/" [] defaults

nbodySizes :: [Integer]
nbodySizes = [1000,5000 .. 66000]

openAccOpts = And [ Set (Variant "OpenAcc") (RuntimeEnv "PGI_ACC_TIME" "1")
                  , Set NoMeaning           (RuntimeEnv "ACC_NOTIFY"   "1") ]
--openMPOpts  = And [ Set (Variant "OpenMP") (RuntimeEnv "PGI_ACC_TIME" "1")
--                  , Set NoMeaning          (RuntimeEnv "ACC_NOTIFY"   "1")
--                  , Set NoMeaning          (RuntimeEnv "PROG_SUFFIX"  "-omp") ]
        
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
