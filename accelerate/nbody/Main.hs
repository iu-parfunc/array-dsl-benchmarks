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
import           Data.Array.Accelerate          ((:.),Z(Z))
#ifdef ACCBACKEND
import qualified ACCBACKEND as Bkend
#else
import qualified Data.Array.Accelerate.CUDA as Bkend
#endif

-- system
import Control.Exception (evaluate)
import Control.Monad     (forM_)
import Prelude                                  as P
import           Data.Char (isSpace)
import qualified Data.Array.Unboxed             as U
import           Data.Time.Clock (getCurrentTime, diffUTCTime)
-- import qualified Data.Array.IO                  as Arr
import           Data.ByteString.Lex.Double (readDouble)
import qualified Data.ByteString.Char8 as B
-- import qualified Data.ByteString.Lazy.Char8 as B
import System.Environment
import System.IO
import System.Mem  (performGC)
import Debug.Trace 

--------------------------------------------------------------------------------

defaultBodies :: Int
defaultBodies = 1000

inputFile = "/tmp/uniform.3dpts"
outputFile = "/tmp/nbody_out.3dpts"

--------------------------------------------------------------------------------

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

writeGeomFile :: FilePath -> A.Vector (Double,Double,Double) -> IO ()
writeGeomFile path arr = do
  hnd <- openFile path WriteMode
  B.hPutStrLn hnd "pbbs_sequencePoint3d"
  forM_ [0..(A.arraySize$ A.arrayShape arr)-1] $ \ ix -> do    
    -- could use bytestring-show or double-conversion packages here:
    let (x,y,z) = A.indexArray arr (Z A.:. ix)
    hPutStr   hnd (show x);  B.hPutStr hnd " "
    hPutStr   hnd (show y);  B.hPutStr hnd " "
    hPutStrLn hnd (show z);  
  hClose hnd
  return ()

main :: IO ()
main = do
  args <- getArgs
  n <- case args of
         [] -> do putStrLn$ "Using default number of bodies: "++show defaultBodies
                  return defaultBodies
         [n] -> return (read n)
         
  putStrLn$ "NBODY: Reading input file..."
  raw <- readGeomFile n inputFile
  putStrLn$ "  Input prefix(3) "++ show(take 3$ U.elems raw)
  putStrLn$ "Done reading, converting to Acc array.."
  let input :: A.Acc (A.Vector (((Double,Double,Double),Double), (Double,Double,Double), (Double,Double,Double)))
#ifdef DEBUG
      input = A.generate (A.index1$ A.constant n) $ \ix ->
                let i,one :: A.Exp Double
                    i = A.fromIntegral $ A.unindex1 ix
                    one = 1 in
                A.lift (((i,i,i),one), (one,one,one), (one,one,one))    
#else    
      input  = A.use input0
      input0 = A.fromIArray $ U.amap (\ pos -> ((pos,1),(1,1,1),(1,1,1))) raw
  evaluate input0
#endif
  performGC
  
  putStrLn$ "Input in CPU memory, starting benchmark..."
  t1 <- getCurrentTime
  output <- evaluate $ Bkend.run $ Naive.calcAccels (A.constant 50) input
  t2 <- getCurrentTime
  let dt = diffUTCTime t2 t1
  putStrLn$ "  Result prefix(3): "++ show(take 3$ A.toList output)
  putStrLn$ "  Result shape "++ show(A.arrayShape output)
  putStrLn$ "SELFTIMED-with-compile: "++ show dt

  putStrLn$ "Writing output file to: "++ outputFile
  writeGeomFile outputFile output
