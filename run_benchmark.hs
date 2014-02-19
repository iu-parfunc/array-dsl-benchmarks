
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
 [ nbody { target="./accelerate/nbody/seq_c/", progname= Just "accelerate-nbody-seqC" }
 , nbody { target="./accelerate/nbody/cilk/", progname= Just "accelerate-nbody-cilk" }
 , nbody { target="./accelerate/nbody/cuda/", progname= Just "accelerate-nbody-cuda" }
 ]
 where 
  nbody = Benchmark { cmdargs=["10000","./accelerate/nbody/makefile_based/uniform.3dpts"], 
                      configs= And[], benchTimeOut= Just 50, target="", progname=Nothing }

--------------------------------------------------------------------------------

-- defaultSettings :: BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
-- defaultSettings spc =
--   And [ Set NoMeaning (RuntimeParam "+RTS -s -qa -RTS")
--       , spc ]

--------------------------------------------------------------------------------
-- Supporting definitions:
--------------------------------------------------------------------------------

-- -- TODO: make this a command-line option:
-- threadSelection :: [Int]
-- threadSelection = unsafePerformIO $ do
--   env <- getEnvironment
--   p   <- getNumProcessors
--   case lookup "THREADS" env of
--     Just ls -> return$ map read $ words ls
--     -- Arbitrary default policy 
--     Nothing
--       | p <= 16   -> return  [1 .. p]
--       | otherwise -> return$ 1 : [2,4 .. p]

-- -- | Add variation from thread count.    
-- varyThreads :: BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
-- varyThreads conf = Or
--   [
--     -- Disabling unthreaded mode:
--     -- conf -- Unthreaded mode.
--     And [
--           -- Set NoMeaning (CompileParam "--ghc-options='-threaded'")
--           Or (map fn threadSelection)
--         , conf ]
--   ]
--  where
--    fn n = Set (Threads n) $ RuntimeParam  ("+RTS -N"++ show n++" -RTS")
