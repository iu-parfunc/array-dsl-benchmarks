{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedStrings #-}
--
-- An N-Body simulation
--

module Main where

import           Data.Array.Accelerate          as A
import           Data.Array.Accelerate          ((:.),Z(Z))
#ifdef ACCBACKEND
import qualified ACCBACKEND as Bkend
#else
#error "Must specify ACCBACKEND CPP variable to build this nbody benchmark."
-- import qualified Data.Array.Accelerate.CUDA as Bkend
#endif
import qualified Data.Array.Accelerate.Interpreter as I
import           Data.Array.Accelerate.BackendClass (runTimed, AccTiming(..))

-- system
import Control.Exception (evaluate)
import Control.Monad     (forM_,when)
import Prelude                                  as P
import           Data.Char (isSpace)
import qualified Data.Array.Unboxed             as U
import qualified Data.List as L
import           Data.Time.Clock (getCurrentTime, diffUTCTime)
-- import qualified Data.Array.IO                  as Arr
import           Data.ByteString.Lex.Double (readDouble)
import qualified Data.ByteString.Char8 as B
-- import qualified Data.ByteString.Lazy.Char8 as B
import System.Environment
import System.IO
import System.Environment (getEnvironment)
import System.Mem  (performGC)
import Data.Maybe (fromJust)
import Debug.Trace 

import Common.Util (plusV)
--------------------------------------------------------------------------------
-- Settings
--------------------------------------------------------------------------------

type R          = Double
type Velocity   = (R, R, R)
type Position   = (R, R, R)
type Accel      = (R, R, R)

defaultInputFile = "./uniform.3dpts"
outputFile = "./nbody_out.3dpts"

--------------------------------------------------------------------------------
-- Reading/writing file data
--------------------------------------------------------------------------------

-- | Read a PBBS geometry file (3D points):
readGeomFile :: Maybe Int -> FilePath -> IO (U.Array Int (Double,Double,Double))
readGeomFile len path = do
  str <- B.readFile path
  let (hd:lines) = B.lines str

  let numlines = length lines
      len2 = case len of
               Just n  -> n
               Nothing -> numlines
                 
  putStrLn$ "Read "++show numlines++" lines from file..."
  when (len2 > numlines)$ error$"Not enough data in file!  Desired: "++show len2++", found "++show numlines
  
  let parsed = P.map parse (P.take len2 lines)
      trim   = B.dropWhile isSpace
      parse ln =
        let Just (x,r1) = readDouble ln
            Just (y,r2) = readDouble (trim r1)
            Just (z,_ ) = readDouble (trim r2)
        in (x,y,z)
  case hd of
    "pbbs_sequencePoint3d" -> return (U.listArray (0,len2 - 1) parsed)
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

--------------------------------------------------------------------------------
-- MAIN script
--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  (n) <- case args of
         []  -> do putStrLn "Using default size for input."
                   return (Just 100)
         [n] -> do putStrLn$ "NBODY size on command line: N="++ show n
                   return (Just $ read n)
  env <- getEnvironment
  let file = case L.lookup "ACCELERATE_INPUT_FILE" env of
              Nothing -> defaultInputFile
              Just s -> s
    
-- Temporary: for debugging we aren't using a file at all:
#ifdef DEBUG
#else
  putStrLn$ "NBODY: Reading requested prefix of input file... "++show n
  raw <- readGeomFile n file
  putStrLn$ "Done reading, converting to Acc array.."
#endif

  let input :: A.Acc (A.Vector Position)
#ifdef DEBUG      
      input = A.generate (A.index1$ A.constant$ fromJust n) $ \ix ->
                let i,one :: A.Exp Double
                    i = A.fromIntegral $ A.unindex1 ix
                    one = 1 in
                A.lift (i,i,i)    
  putStrLn$ "  Input prefix(4) "++ show(P.take 3$ A.toList $ I.run input)
#else    
      input  = A.use input0
      input0 = A.fromIArray $ raw 
  putStrLn$ "  Input prefix(4) "++ show(P.take 3$ U.elems raw)
  evaluate input0
#endif
  performGC
  putStrLn$ "Input in CPU memory, starting benchmark..."
  output <- evaluate $ Bkend.run $ calcAccels input
  (times,output) <- runTimed Bkend.defaultBackend Nothing (calcAccels input)                    
  let AccTiming{compileTime,runTime,copyTime} = times
  putStrLn$ "  Result prefix(4): "++ show(P.take 3$ A.toList output)
  putStrLn$ "  Result shape "++ show(A.arrayShape output)
  putStrLn$ "  All timing: "++ show times
  putStrLn$ "JITTIME: "++ show compileTime
  putStrLn$ "SELFTIMED: "++ show (runTime + copyTime)
  putStrLn$ "Writing output file to: "++ outputFile
  writeGeomFile outputFile output


----------------------------------------------------------------------------------------------------
-- The actual benchmark code:
----------------------------------------------------------------------------------------------------

calcAccels :: A.Acc (A.Vector Position) -> A.Acc (A.Vector Accel)
calcAccels bodies
  = let n       = A.size bodies

        cols    = A.replicate (lift $ Z :. n :. All) bodies
        rows    = A.replicate (lift $ Z :. All :. n) bodies

    in
    A.fold plusV (constant (0,0,0)) $ A.zipWith (accel) rows cols


-- Acceleration ----------------------------------------------------------------
--
-- | Calculate the acceleration on a point due to some other point as an inverse
--   separation-squared relation.
--
accel   :: Exp Position           -- ^ The point being accelerated
        -> Exp Position           -- ^ Neighbouring point
        -> Exp Accel
accel body1 body2
  =
--    (theaccel :: Accel)
    (A.not (x1 ==* x2 &&* y1 ==* y2 &&* z1 ==* z2)) ?
    (theaccel, lift ((0, 0, 0) :: Accel))
  where
    theaccel = lift (aabs * dx / r , aabs * dy / r, aabs * dz / r)
    (x1, y1, z1) = unlift $ body1
    (x2, y2, z2) = unlift $ body2
    m1          = 1
    m2          = 1

    dx          = x2 - x1
    dy          = y2 - y1
    dz          = z2 - z1
    rsqr        = (dx * dx) + (dy * dy) + (dz * dz)
    aabs        = (m1 * m2) / rsqr
    r           = sqrt rsqr




--------------------------------------------------------------------------------
-- OLD Main
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
