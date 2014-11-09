{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
--
-- A k-means clustering implementation.
-- Run the generate-samples program first to create some random data.
--

module Main where

import Kmeans

import Prelude                                                  as P
import Control.Applicative                                      ( (<$>), (<*>) )
import Control.Monad                                            ( unless )
import Data.Binary                                              ( decodeFile )
import Data.Time.Clock
import System.Directory
import System.Environment

import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.BackendClass
#ifndef NOSIMPLE
import Data.Array.Accelerate.BackendKit.CompilerPipeline        ( phase0, phase1 )
#endif
#ifdef ACCBACKEND
import qualified ACCBACKEND                                     as A
#else
#error "Must specify ACCBACKEND CPP variable to build this nbody benchmark."
#endif
#ifdef EXTRAINITCUDA
import qualified Foreign.CUDA.Driver                            as CUDA
#endif


main :: IO ()
main = do
  args          <- getArgs
  env           <- getEnvironment
#ifdef EXTRAINITCUDA
  CUDA.initialise []
#endif

  let pointsFile        = case lookup "ACCELERATE_POINTS_FILE" env of
                            Nothing -> "points.bin"
                            Just fp -> fp
      clustersFile      = case lookup "ACCELERATE_CLUSTERS_FILE" env of
                            Nothing -> "clusters"
                            Just fp -> fp

  inputs        <- (&&) <$> doesFileExist pointsFile
                        <*> doesFileExist clustersFile
  unless inputs $ do
    error "Run the GenSamples program first to generate random data"

  points'       <- decodeFile pointsFile
  initial'      <- read `fmap` readFile clustersFile

  let nclusters = P.length initial'
      npoints   = case args of
                    []                          -> P.length points'
                    [x] | [(x',[])] <- reads x  -> x'
                    _                           -> error "Usage: kmeans [#points]"

      initial   = A.fromList (Z:.nclusters) initial'
      points    = A.fromList (Z:.npoints)   points'

      step :: Acc (Scalar Bool, Vector (Cluster Float))
      step      = kmeans1 (use points) (use initial)

  putStrLn $ "  Number of points: " P.++ show npoints
  putStrLn $ "  Number of clusters: " P.++ show nclusters

  -- Run the benchmark and get timing measuremnts
  --
  tBegin        <- getCurrentTime
#ifndef NOSIMPLE
  (time,res)    <- runTimedSimple A.defaultBackend Nothing A.defaultTrafoConfig (phase1 $ phase0 $ step)
#else
  (time,res)    <- runTimed       A.defaultBackend Nothing A.defaultTrafoConfig step
#endif
  tEnd          <- res `seq` getCurrentTime

  let AccTiming{..} = time
  putStrLn $ "  Result: " P.++ show res
  putStrLn $ "  All timing: " P.++ show time
  putStrLn $ "  Total time for runTimed " P.++ show (diffUTCTime tEnd tBegin)
  putStrLn $ "JITTIME: "   P.++ show compileTime
  putStrLn $ "SELFTIMED: " P.++ show (runTime + copyTime)

