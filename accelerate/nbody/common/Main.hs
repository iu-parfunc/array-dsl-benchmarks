{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
--
-- An N-Body simulation
--

-- friends
import IO
import Common.Body
import Common.World
import Random.Array
-- import qualified Solver.Naive1                                  as Naive1
import qualified Solver.Naive2                                  as Naive2
-- import qualified Solver.BarnsHut                                as BarnsHut

-- system
import Prelude                                                  as P
import Data.Time.Clock
import System.Mem
import System.Environment

import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.BackendClass
import Data.Array.Accelerate.BackendKit.CompilerPipeline        ( phase0, phase1 )
#ifdef ACCBACKEND
import qualified ACCBACKEND                                     as A
#else
#error "Must specify ACCBACKEND CPP variable to build this nbody benchmark."
#endif


-- Settings
-- --------

defaultInputFile, defaultOutputFile :: FilePath
defaultInputFile        = "uniform.3dpts"
defaultOutputFile       = "nbody_out.3dpts"

-- Main
-- ----
main :: IO ()
main = do
  args          <- getArgs
  env           <- getEnvironment

  let nBodies   = case args of
                    []                          -> 100
                    [x] | [(x',[])] <- reads x  -> x'
                    _                           -> error "Usage: nbody [#points]"

      inputFile = case lookup "ACCELERATE_INPUT_FILE" env of
                    Nothing -> defaultInputFile
                    Just fp -> fp

  -- Generate the initial data. We read the points from file and generate some
  -- random masses (but based on a constant seed).
  --
  positions     <- readGeomFile (Just nBodies) inputFile
  let mass      = 40
      speed     = 1

      masses    = randomArray (uniformR (1, mass)) (Z :. nBodies)
      bodies    = A.run
                $ A.map (setStartVelOfBody (constant speed))
                $ A.zipWith setMassOfBody (A.use masses)
                $ A.map unitBody (A.use positions)

  putStrLn $ "  Input prefix[3] ... " P.++ show (P.take 3 (toList bodies))
  performGC

  -- Okay, now try to compute the n-body program
  --
  let epsilon   = 40
      dt        = fromList Z [0.1]

      solver    = Naive2.calcAccels epsilon
      step      = advanceBodies solver (use dt) (use bodies)
      step'     = phase1 $ phase0 $ step

  -- Run the benchmark and get timing measuremnts
  --
  tBegin        <- getCurrentTime
#ifndef NOSIMPLE
  (time,res)    <- runTimedSimple A.defaultBackend Nothing A.defaultTrafoConfig step'
#else
  (time,res)    <- runTimed       A.defaultBackend Nothing A.defaultTrafoConfig step
--  putStrLn $ "  Output prefix[3] ... " P.++ show (P.take 3 (toList res))
#endif
  tEnd          <- res `seq` getCurrentTime

  let AccTiming{..} = time
  putStrLn $ "  All timing: " P.++ show time
  putStrLn $ "  Total time for runTimed " P.++ show (diffUTCTime tEnd tBegin)
  putStrLn $ "JITTIME: "   P.++ show compileTime
  putStrLn $ "SELFTIMED: " P.++ show (runTime + copyTime)



{--
main :: IO ()
main
  = do  beginMonitoring
        argv                    <- getArgs
        (conf, opts, rest)      <- parseArgs options defaults header footer argv

        let solver      = case get configSolver conf of
                            Naive1      -> Naive1.calcAccels
                            Naive2      -> Naive2.calcAccels
                            BarnsHut    -> BarnsHut.calcAccels

            n           = get configBodyCount conf
            size        = get configWindowSize conf
            fps         = get configRate conf
            epsilon     = get configEpsilon conf
            mass        = get configBodyMass conf
            radius      = get configStartDiscSize conf
            backend     = get optBackend opts

            -- Generate random particle positions in a disc layout centred at
            -- the origin. Start the system rotating with particle speed
            -- proportional to distance from the origin
            --
            positions   = randomArray (cloud (size,size) radius) (Z :. n)
            masses      = randomArray (uniformR (1, mass)) (Z :. n)

            bodies      = run backend
                        $ A.map (setStartVelOfBody . constant $ get configStartSpeed conf)
                        $ A.zipWith setMassOfBody (A.use masses)
                        $ A.map unitBody (A.use positions)

            -- The initial simulation state
            --
            universe    = initialise world
            world       = World { worldBodies   = bodies
                                , worldSteps    = 0
                                , worldTime     = 0 }

            -- Advancing the simulation
            --
            advance     = advanceWorld step
            step        = P.curry
                        $ run1 backend
                        $ A.uncurry
                        $ advanceBodies (solver $ constant epsilon)


        -- Forward unto dawn
        --
        runTests opts rest
          $ makeTests step

        runBenchmarks opts rest
          [ bench "n-body" $ whnf (advance 0.1) world ]

        runInteractive opts rest
          $ play
              (InWindow "N-Body" (size, size) (10, 10))         -- window size & position
              black                                             -- background colour
              fps                                               -- number of simulation steps per second
              universe                                          -- initial world
              (draw conf)                                       -- fn to convert a world into a picture
              react                                             -- fn to handle input events
              (simulate advance)                                -- fn to advance the world
--}

