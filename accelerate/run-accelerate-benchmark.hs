{-# LANGUAGE NamedFieldPuns #-}

{- | The benchmarking script:

USAGE:

   ./run_benchmark [mode] [hsbencher options]

Where mode is 'desktop', 'server', or 'quick'.

-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Main where

import Data.Monoid
import           GHC.Conc           (getNumProcessors)
import           System.Console.GetOpt
import           System.Environment (getEnvironment, getArgs, withArgs)
import           System.IO.Unsafe   (unsafePerformIO)

import HSBencher (defaultMainModifyConfig, all_cli_options, 
                  BenchSpace(..), Benchmark(..), ParamSetting(..), DefaultParamMeaning(..),
                  Config(..), SomePlugin(..))
import HSBencher.Backend.Fusion  (defaultFusionPlugin)
import HSBencher.Backend.Dribble (defaultDribblePlugin)
import HSBencher.Harvesters (customTagHarvesterDouble)

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
--     defaultMainWithBechmarks bls_desktop
     defaultMainModifyConfig $ \ conf ->
       conf{ benchlist  = bls_desktop
           , runTimeOut = Just 1000 -- Erk... need a separate compile timeout.
--            , buildMethods = [makeMethod]
           , plugIns   = [ SomePlugin defaultFusionPlugin,
                           SomePlugin defaultDribblePlugin ]

           -- [2014.07.08] RRN: This method isn't yet perfect because
           -- we need to do a sum over multiple occurrences of
           -- SUBSELFTIMED or SUBJITTIME:
           , harvesters = customTagHarvesterDouble "COMPILETIME_CUDA" `mappend`
                          customTagHarvesterDouble "COMPILETIME_CUDA1"   `mappend`
                          customTagHarvesterDouble "COMPILETIME_CILK"   `mappend`
                          customTagHarvesterDouble "RUNTIME_C"   `mappend`
                          customTagHarvesterDouble "RUNTIME_CUDA1"   `mappend`
                          customTagHarvesterDouble "RUNTIME_CUDA2"   `mappend`
                          customTagHarvesterDouble "RUNTIME_CILK"   `mappend`
                          customTagHarvesterDouble "COMPILETIME_TOTAL"   `mappend`
                          customTagHarvesterDouble "RUNTIME_TOTAL"   `mappend`
                          harvesters conf
           }

    
--------------------------------------------------------------------------------
-- Here are the actual benchmarks:
--------------------------------------------------------------------------------

-- TODO: create different settings for these based on quick vs. big runs:
nbody_args :: [Integer]
-- nbody_args = [10000, 15000, 25000 ]
-- nbody_args = [1000,2000 .. 66000] -- FIXME: too many datapoints [2014.07.02]
nbody_args = [1000,5000 .. 66000] 

blackscholes_args :: [Integer]
blackscholes_args = [1000000, 2000000, 10000000, 20000000]

mandel_args :: [Integer]
mandel_args = [100,200,500,1000,2000,3000]

kmeans_args :: [Integer]
kmeans_args = [10000, 20000 .. 100000]

bls_desktop :: [Benchmark DefaultParamMeaning]
bls_desktop =
  ------ Traditional benchmarks ------
  allBlackscholes
  ++ allNBodies
  ++ allMandel
  ++ allKmeans

  ------ Multi-device benchmarks ------
  -- ++ allMultiNBodies        
  -- ++ allMultiBlackscholes   

  ------ Micro-benchmarks ------
  ++ allScaleFlops
  ++ allScaleFlops2
  ++ allReduces 
  
 where 
  -- Argument variation:
  ----------------------------------------

  -- Building/aggregating all variants:
  ----------------------------------------

  root = "./"

  allReduces = [ baseline { cmdargs= [show (round (1000000 * sz)::Integer), mode],
                            configs= And[ Set (Variant "cuda") (RuntimeEnv "IGNORE_THIS" "")],
                            target= root++"reduce/cuda/reduce-cuda.cabal",
                            progname= Just "accelerate-reduce-microbench" }
               | sz <- (0.25 : 0.5 : [1..16]) :: [Double]
               , mode <- ["awhile", "loop"]]

  allBlackscholes = concat [ allvariants (blackscholes (show arg))
                           | arg <- blackscholes_args ]

  allMandel = concat [ allvariants (mandel (show arg))
                     | arg <- mandel_args ]

  allNBodies = concat [ allvariants (nbody (show arg)) 
                      | arg <- nbody_args ]
--               concat [ allvariants (nbody_plusplus (show arg)) 
--                      | arg <- nbody_args ]

  allKmeans = concat [ allvariants (kmeans (show arg)) | arg <- kmeans_args ]

  -- Vary the size of the big arithmetic expression generated:
  allScaleFlops = let sz = "2000000" in 
                  concat [ allvariants (scaleFlops args) 
                         | args <- ["0",sz] : [ [show (2^n::Integer), sz] 
                                              | n <- [0..10::Integer]]
                         ]
  -- Create a sequential inner loop which performs a variable amount of arithmetic.
  allScaleFlops2 = concat [ allvariants (scaleFlops2 args) 
                          | sz <- ["1000000", "2000000"] -- Vary array size.
                          , args <- ["0",sz] : [ [show (2^n::Integer), sz] 
                                               | n <- [0..13::Integer]] 
                            -- Vary inner loop trip-count.
                          ]

  -------------------------------------------
  -- Define each benchmark's characteristics:
  -------------------------------------------

  nbody size var = 
              baseline { cmdargs=[size], 
                         configs= And[ Set (Variant var)
                                        (RuntimeEnv "ACCELERATE_INPUT_FILE"
                                         (root++"nbody/points/uniform.3dpts"))],
                         target= root++"nbody", -- Just the root
                         progname= Just "accelerate-nbody-float" }

  mandel size var = 
              baseline { cmdargs=[size], 
                         configs= And[ Set (Variant var) (RuntimeEnv "IGNORE_THIS" "")],
                         target= root++"mandelbrot", -- Just the root
                         progname= Just "accelerate-mandel" }

--  nbody_plusplus size var = 
--              baseline { cmdargs=[size], 
--                         configs= And[ Set (Variant var)
--                                        (RuntimeEnv "ACCELERATE_INPUT_FILE"
--                                         (root++"nbody_plusplus/makefile_based/uniform.3dpts"))],
--                         target= root++"nbody_plusplus", -- Just the root
--                         progname= Just "accelerate-nbody_plusplus-float" }

  blackscholes size var = 
              baseline { cmdargs=[size], 
                         configs= And[ -- Set (Variant var) (RuntimeEnv "IGNORE_THIS" ""),
                                       Or [ Set NoMeaning (CompileParam "--ghc-options=-DUSE_FOLD")
--                                          , Set NoMeaning (CompileParam "")
                                          ],
                                       Or [ Set (Variant (var++"-replicate")) (CompileParam "--ghc-options=-DUSE_REPLICATE")
                                          , Set (Variant (var)) (CompileParam "")
                                          ]
                                     ],
                         target= root++"blackscholes", -- Just the root
                         progname= Just "accelerate-blackscholes" }

  kmeans size var =
    baseline { cmdargs  = [size]
             , configs  = And [ Set (Variant var) (RuntimeEnv "ACCELERATE_POINTS_FILE"   (root ++ "kmeans/points/points.bin"))
                              , Set (Variant var) (RuntimeEnv "ACCELERATE_CLUSTERS_FILE" (root ++ "kmeans/points/clusters"))
                              ]
             , target   = root ++ "kmeans"
             , progname = Just "accelerate-kmeans"
             }

  scaleFlops :: [String] -> String -> Benchmark DefaultParamMeaning
  scaleFlops args var = 
      baseline { cmdargs=args, 
                 configs= And[ Set (Variant var) (CompileParam "")],
                 target= root++"scale_flops", -- Just the root
                 progname= Just "accelerate-scaleFlops" }

  scaleFlops2 :: [String] -> String -> Benchmark DefaultParamMeaning
  scaleFlops2 args var = 
      baseline { cmdargs=args, 
                 configs= And[ Set (Variant var) (CompileParam "")],
                 target= root++"scale_flops2", -- Just the root
                 progname= Just "accelerate-scaleFlops2" }

  -- Helpers
  ----------------------------------------

  -- Run with all of the backends:
  allvariants fn = 
    let dirroot = target (fn "seqC") in 
    [ (fn "seqC") { target= dirroot++"/seq_c/" }
    , (fn "cuda") { target= dirroot++"/cuda/"  }
    , (fn "1gpu") { target= dirroot++"/1gpu/"  }

    , varyCilkThreads   threadSelection  $ (fn "cilk") { target= dirroot++"/cilk/"  }

    -- TODO: Vary threads for CPU/GPU:
    , (fn "cpugpu")   { target= dirroot++"/cpugpu/"  }
    , (fn "2gpu")     { target= dirroot++"/2gpu/"  }
-- We shouldn't be varying fission factor right ? 
--    , varyFission fissionSelection $ (fn "cpugpu")   { target= dirroot++"/cpugpu/"  }
--    , varyFission fissionSelection $ (fn "2gpu")     { target= dirroot++"/2gpu/"  }
    ]

  baseline = Benchmark { cmdargs=[], configs= And[], benchTimeOut=Just defaultTimeout, target="", progname=Nothing }



-- Use 50 seconds as the default timeout:
-- JS: tweaks this temporarily to 150
defaultTimeout :: Double
defaultTimeout = 150

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

fissionSelection :: [Int]
fissionSelection = unsafePerformIO $ do
  p   <- getNumProcessors
  return $ takeWhile (<p) [ 2^(n::Int) | n <- [1..]] ++ 
           [p, 2*p, 4*p]

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
