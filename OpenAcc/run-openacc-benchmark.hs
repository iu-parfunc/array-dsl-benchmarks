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
  [ mkBenchmark "./nbody/" [] defaults
--  , mkBenchmark "./blackscholes/" [] defaults
  ]

defaults = And $ [ Set NoMeaning (RuntimeEnv "PGI_ACC_TIME" "1")
                 , Set NoMeaning (RuntimeEnv "ACC_NOTIFY"   "1") ] ++
                 [ Set (Variant "NumBodies")  (RuntimeEnv "NUM_BODIES"  (show x)) | x <- [10000, 20000, 40000, 80000, 160000]]

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

