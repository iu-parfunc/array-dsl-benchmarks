{-# LANGUAGE NamedFieldPuns #-}

{- | The benchmarking script:

USAGE:

   ./run_benchmark [mode] [hsbencher options]

Where mode is 'desktop', 'server', or 'quick'.

-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Main where

import qualified Data.Set as S

import GHC.Conc           (getNumProcessors)
import System.Environment (getEnvironment, getArgs, withArgs)
import System.IO.Unsafe   (unsafePerformIO)
import System.Console.GetOpt

-- import HSBencher.Types(BenchSpace(..), Benchmark(..), ParamSetting(..), DefaultParamMeaning(..),
--                        -- compileOptsOnly, enumerateBenchSpace, toCompileFlags,
--                        -- makeBuildID, BuildID,
--                        mkBenchmark
--                       )
import HSBencher (defaultMainWithBechmarks, all_cli_options, fullUsageInfo,
                  BenchSpace(..), Benchmark(..), ParamSetting(..), DefaultParamMeaning(..),
                  mkBenchmark
                  )

--------------------------------------------------------------------------------
-- Main Script
--------------------------------------------------------------------------------

-- data Mode = Server | Desktop | Quick      deriving (Show,Eq)
data Flag = Help
--          | SetMode Mode
--          | SetSched Sched
          deriving (Show,Eq)

options :: [OptDescr Flag]
options =
     [ Option ['h'] ["help"] (NoArg Help)              "report this help message"
     ]

main :: IO ()
main = do
  args <- getArgs
  let (opts,nonopts,unrecog,errs) = getOpt' Permute options args
  let help1 = usageInfo ("USAGE: run_benchmark [options]\n"++
                        "\nFirst, specific options for this script are:\n")
                options
      help2 = usageInfo (help1++"\nAlso use the generic HSBencher options below:\n")
                        (concat $ map snd all_cli_options)

  if Help `elem` opts || errs /= [] then
    error help2
   else do
    let passthru = nonopts ++ unrecog
    putStrLn$ "  [Note: passing through options to HSBencher]: "++unwords passthru
    withArgs passthru $ 
     defaultMainWithBechmarks bls_desktop
    
--------------------------------------------------------------------------------
-- Here are the actual benchmarks:
--------------------------------------------------------------------------------

bls_desktop :: [Benchmark DefaultParamMeaning]
bls_desktop = 
  ------ Traditional benchmarks ------
  allNBodies 
  ++ allBlackscholes

  ------ Multi-device benchmarks ------
  ++ allMultiNBodies        
  ++ allMultiBlackscholes   

  ------ Micro-benchmarks ------
  ++ allScaleFlops
  ++ allScaleFlops2
  ++ allReduces 
  
 where 
  -- Argument variation:
  ----------------------------------------

  -- nbody_args = [10000, 15000, 25000 ]
  -- nbody_args = [1000,2000 .. 66000] -- FIXME: too many datapoints [2014.07.02]
  nbody_args = [1000,5000 .. 66000] 

  blackscholes_args = [100000, 1000000, 2000000, 10000000]

  -- Building/aggregating all variants:
  ----------------------------------------

  allReduces = [ baseline { cmdargs= [show $ round $  1000000 * sz, mode],
                            configs= And[ Set (Variant "cuda") (RuntimeEnv "IGNORE_THIS" "")],
                            target= "./accelerate/reduce/cuda/reduce-cuda.cabal",
                            progname= Just "accelerate-reduce-microbench" }
               | sz <- (0.25:0.5:[1..16]) 
               , mode <- ["awhile", "loop"]]

  allBlackscholes = concat [ allthree (blackscholes (show arg))
                           | arg <- blackscholes_args ]

  allMultiBlackscholes = 
       [ baseline { cmdargs=[ show arg ], 
                    configs= And[ Set (Variant "cpugpu")
                                   (RuntimeEnv "IGNORE_THIS" "")],
                    target= "./accelerate/blackscholes_temp/cpugpu",
                    progname= Just "accelerate-blackscholes-cpugpu" }
       | arg <- blackscholes_args ]

  allNBodies = concat [ allthree (nbody (show arg)) 
                      | arg <- nbody_args ]

  allMultiNBodies = 
       [ baseline { cmdargs=[ show arg ], 
                    configs= And[ Set (Variant "cpugpu")
                                  (RuntimeEnv "ACCELERATE_INPUT_FILE"
                                   "./accelerate/nbody_temp/common/uniform.3dpts")],
                    target= "./accelerate/nbody_temp/cpugpu",
                    progname= Just "accelerate-nbody-cpugpu" }
       | arg <- nbody_args ]
                
  -- Vary the size of the big arithmetic expression generated:
  allScaleFlops = let sz = "2000000" in 
                  concat [ allthree (scaleFlops args) 
                         | args <- ["0",sz] : [ [show (2^n), sz] | n <- [0..10]]
                         ]
  -- Create a sequential inner loop which performs a variable amount of arithmetic.
  allScaleFlops2 = concat [ allthree (scaleFlops2 args) 
                          | sz <- ["1000000", "2000000"] -- Vary array size.
                          , args <- ["0",sz] : [ [show (2^n), sz] | n <- [0..13]] 
                            -- Vary inner loop trip-count.
                          ]

  -------------------------------------------
  -- Define each benchmark's characteristics:
  -------------------------------------------

  nbody size var = 
              baseline { cmdargs=[size], 
                         configs= And[ Set (Variant var)
                                        (RuntimeEnv "ACCELERATE_INPUT_FILE"
                                         "./accelerate/nbody/makefile_based/uniform.3dpts")],
                         target= "./accelerate/nbody", -- Just the root
                         progname= Just "accelerate-nbody-float" }

  blackscholes size var = 
              baseline { cmdargs=[size], 
                         configs= And[ Set (Variant var)
                                        (RuntimeEnv "IGNORE_THIS" "")],
                         target= "./accelerate/blackscholes", -- Just the root
                         progname= Just "accelerate-blackscholes" }

  scaleFlops args var = 
      baseline { cmdargs=args, 
                 configs= And[ Set (Variant var) (CompileParam "")],
                 target= "./accelerate/scale_flops", -- Just the root
                 progname= Just "accelerate-scaleFlops" }

  scaleFlops2 args var = 
      baseline { cmdargs=args, 
                 configs= And[ Set (Variant var) (CompileParam "")],
                 target= "./accelerate/scale_flops2", -- Just the root
                 progname= Just "accelerate-scaleFlops2" }

  -- Helpers
  ----------------------------------------

  -- Run with all of the backends:
  allthree fn = 
    let root = target (fn "seqC") in 
    [ (fn "seqC") { target= root++"/seq_c/" }
    , (fn "cuda") { target= root++"/cuda/"  }
    , varyCilkThreads threadSelection $ (fn "cilk") { target= root++"/cilk/"  }
    , varyFission threadSelection $ (fn "fission1") { target= root++"/fission1/" }
    , varyFission threadSelection $ (fn "spmd2")    { target= root++"/spmd2/" }
    ]

  baseline = Benchmark { cmdargs=[], configs= And[], benchTimeOut=Just defaultTimeout, target="", progname=Nothing }



-- Use 50 seconds as the default timeout:
defaultTimeout :: Double
defaultTimeout = 50

--------------------------------------------------------------------------------

-- defaultSettings :: BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
-- defaultSettings spc =
--   And [ Set NoMeaning (RuntimeParam "+RTS -s -qa -RTS")
--       , spc ]

--------------------------------------------------------------------------------
-- Supporting definitions:
--------------------------------------------------------------------------------

-- -- TODO: make this a command-line option:
threadSelection :: [Int]
threadSelection = unsafePerformIO $ do
  env <- getEnvironment
  p   <- getNumProcessors
  case lookup "THREADS" env of
    Just ls -> return$ map read $ words ls
    -- Arbitrary default policy 
    Nothing
      | p <= 16   -> return  [1 .. p]
      | otherwise -> return$ 1 : [2,4 .. p]

varyCilkThreads :: [Int] -> Benchmark DefaultParamMeaning -> Benchmark DefaultParamMeaning
varyCilkThreads settings bench@Benchmark{configs} = 
  bench { configs= And [configs, 
                       Or [ Set (Threads n) (RuntimeEnv "CILK_NWORKERS" (show n))
                          | n <- settings ]] }

varyFission :: [Int] -> Benchmark DefaultParamMeaning -> Benchmark DefaultParamMeaning
varyFission settings bench@Benchmark{configs} = 
  bench { configs= And [configs, 
                       Or [ Set (Threads n) (RuntimeEnv "ACC_FISSION_FACTOR" (show n))
                          | n <- settings ]] }

