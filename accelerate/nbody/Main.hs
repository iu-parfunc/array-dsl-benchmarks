{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
--
-- An N-Body simulation
--

-- friends
-- import Config
import Common.Body
-- import Common.World
-- import Random.Array
-- import Random.Position                          
import qualified Solver.Naive                   as Naive
import qualified Solver.BarnsHut                as BarnsHut

import qualified Data.Array.Accelerate          as A
#ifdef ACCBACKEND
import qualified ACCBACKEND as Bkend
#else
import qualified Data.Array.Accelerate.CUDA as Bkend
#endif

-- system
import Control.Exception (evaluate)
import Prelude                                  as P
import           Data.Char (isSpace)
import qualified Data.Array.Unboxed             as U
import           Data.Time.Clock (getCurrentTime, diffUTCTime)
-- import qualified Data.Array.IO                  as Arr
import           Data.ByteString.Lex.Double (readDouble)
import qualified Data.ByteString.Char8 as B
-- import qualified Data.ByteString.Lazy.Char8 as B
import System.Environment
import System.Mem  (performGC)
import Debug.Trace 


-- import Data.Label
-- import System.Random.MWC                        ( uniformR )
-- import Criterion                                ( bench, whnf, runBenchmark )
-- import Criterion.Monad                          ( withConfig )
-- import Criterion.Analysis                       ( analyseMean )
-- import Criterion.Environment                    ( measureEnvironment )

-- main :: IO ()
-- main
--   = do  (conf, cconf, nops)     <- parseArgs =<< getArgs

--         let solver      = case get configSolver conf of
--                             Naive       -> Naive.calcAccels
--                             BarnsHut    -> BarnsHut.calcAccels

--             n           = get configBodyCount conf
--             epsilon     = get configEpsilon conf

--             -- Generate random particle positions in a disc layout centred at
--             -- the origin. Start the system rotating with particle speed
--             -- proportional to distance from the origin
--             --
--             positions   = randomArrayOf (disc (0,0) (get configStartDiscSize conf)) (Z :. n)
--             masses      = randomArrayOf (\_ -> uniformR (1, get configBodyMass conf)) (Z :. n)

--             bodies      = run conf
--                         $ A.map (setStartVelOfBody . constant $ get configStartSpeed conf)
--                         $ A.zipWith setMassOfBody (A.use masses)
--                         $ A.map (A.uncurry unitBody)
--                         $ A.use positions

--             -- The initial simulation state
--             --
--             world       = World { worldBodies   = bodies
--                                 , worldSteps    = 0
--                                 , worldTime     = 0 }

--             -- Advancing the simulation
--             --
--             advance     = advanceWorld step
--             step        = P.curry
--                         $ run1 conf
--                         $ A.uncurry
--                         $ advanceBodies (solver $ constant epsilon)

--         -- Forward unto dawn
--         --
--         mean <- withConfig cconf $ measureEnvironment >>= flip runBenchmark (whnf (advance 0.1) world) >>= flip analyseMean 100
--         putStrLn $ "SELFTIMED: " ++ show mean


-- | Read a PBBS geometry file (3D points):
readGeomFile :: Int -> FilePath -> IO (U.Array Int (Double,Double,Double))
readGeomFile len path = do
  str <- B.readFile path
  let (hd:lines) = B.lines str
      parsed = map parse (take len lines)
--      len    = length lines
      trim   = B.dropWhile isSpace
      parse ln =
        let Just (x,r1) = readDouble ln
--            Just (y,r2) = trace ("READING REST: "++show r1) $ readDouble r1
            Just (y,r2) = readDouble (trim r1)
            Just (z,_ ) = readDouble (trim r2)
        in (x,y,z)
  case hd of
    "pbbs_sequencePoint3d" -> return (U.listArray (0,len-1) parsed)
    oth -> error$"Expected header line (pbbs_sequencePoint3d) got: "++show oth

defaultBodies :: Int
defaultBodies = 1000

main :: IO ()
main = do
  args <- getArgs
  n <- case args of
         [] -> do putStrLn$ "Using default number of bodies: "++show defaultBodies
                  return defaultBodies
         [n] -> return (read n)
         
  putStrLn$ "Reading input file..."
  raw <- readGeomFile n "/tmp/uniform.3dpts"
  putStrLn$ "  Prefix "++ show(take 3$ U.elems raw)
  putStrLn$ "Done reading (prefix "++ show(take 3$ U.elems raw)++") converting to Acc array.."  
  let inputs = A.fromIArray $ U.amap (\ pos -> ((pos,1),(1,1,1),(1,1,1))) raw
  evaluate inputs
  performGC
  putStrLn$ "Input in CPU memory, starting benchmark..."
  t1 <- getCurrentTime
  output <- evaluate $ Bkend.run $ Naive.calcAccels (A.constant 50) (A.use inputs)
  t2 <- getCurrentTime
  let dt = diffUTCTime t2 t1
  putStrLn$ "  Prefix "++ show(take 3$ A.toList output)
  putStrLn$ "SELFTIMED: "++ show dt

