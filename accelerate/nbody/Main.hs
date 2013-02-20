--
-- An N-Body simulation
--

-- friends
import Config
import Common.Body
import Common.World
import Random.Array
import Random.Position
import qualified Solver.Naive                   as Naive
import qualified Solver.BarnsHut                as BarnsHut

import Data.Array.Accelerate                    as A hiding ( size )

-- system
import Prelude                                  as P
import Data.Label
import System.Environment
import System.Random.MWC                        ( uniformR )
import Criterion                                ( bench, whnf, runBenchmark )
import Criterion.Monad                          ( withConfig )
import Criterion.Analysis                       ( analyseMean )
import Criterion.Environment                    ( measureEnvironment )

main :: IO ()
main
  = do  (conf, cconf, nops)     <- parseArgs =<< getArgs

        let solver      = case get configSolver conf of
                            Naive       -> Naive.calcAccels
                            BarnsHut    -> BarnsHut.calcAccels

            n           = get configBodyCount conf
            epsilon     = get configEpsilon conf

            -- Generate random particle positions in a disc layout centred at
            -- the origin. Start the system rotating with particle speed
            -- proportional to distance from the origin
            --
            positions   = randomArrayOf (disc (0,0) (get configStartDiscSize conf)) (Z :. n)
            masses      = randomArrayOf (\_ -> uniformR (1, get configBodyMass conf)) (Z :. n)

            bodies      = run conf
                        $ A.map (setStartVelOfBody . constant $ get configStartSpeed conf)
                        $ A.zipWith setMassOfBody (A.use masses)
                        $ A.map (A.uncurry unitBody)
                        $ A.use positions

            -- The initial simulation state
            --
            world       = World { worldBodies   = bodies
                                , worldSteps    = 0
                                , worldTime     = 0 }

            -- Advancing the simulation
            --
            advance     = advanceWorld step
            step        = P.curry
                        $ run1 conf
                        $ A.uncurry
                        $ advanceBodies (solver $ constant epsilon)

        -- Forward unto dawn
        --
        mean <- withConfig cconf $ measureEnvironment >>= flip runBenchmark (whnf (advance 0.1) world) >>= flip analyseMean 100
        putStrLn $ "SELFTIMED: " ++ show mean

