{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedStrings #-}
--
-- An N-Body simulation
--

module Main where

import           Data.Array.Accelerate          as A hiding ((++))
import           Data.Array.Accelerate          ((:.),Z(Z))
#ifdef ACCBACKEND
import qualified ACCBACKEND as Bkend
#else
#error "Must specify ACCBACKEND CPP variable to build this nbody benchmark."
-- import qualified Data.Array.Accelerate.CUDA as Bkend
#endif
import qualified Data.Array.Accelerate.Interpreter as I
import           Data.Array.Accelerate.BackendClass (runTimed, runTimedSimple, AccTiming(..))
import Data.Array.Accelerate.BackendKit.CompilerPipeline (phase0, phase1)

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

-- Temp hack: 
#ifdef EXTRAINITCUDA
import Foreign.CUDA.Driver (initialise)
#endif

--------------------------------------------------------------------------------
-- Settings
--------------------------------------------------------------------------------

type R          = Float
type Velocity   = (R, R, R)
type Position   = (R, R, R)
type Accel      = (R, R, R)

defaultInputFile = "./uniform.3dpts"
outputFile = "./nbody_out.3dpts"

fromDouble :: Fractional a => Double -> a
fromDouble = fromRational . toRational

--------------------------------------------------------------------------------
-- Reading/writing file data
--------------------------------------------------------------------------------

-- | Read a PBBS geometry file (3D points):
readGeomFile :: Maybe Int -> FilePath -> IO (U.Array Int (R,R,R))
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
        in (fromDouble x, fromDouble y, fromDouble z)
  case hd of
    "pbbs_sequencePoint3d" -> return (U.listArray (0,len2 - 1) parsed)
    oth -> error$"Expected header line (pbbs_sequencePoint3d) got: "++show oth

writeGeomFile :: FilePath -> A.Vector (R,R,R) -> IO ()
-- writeGeomFile :: FilePath -> A.Vector (Double,Double,Double) -> IO ()
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
#ifndef DEBUG
  putStrLn$ "NBODY: Reading requested prefix of input file... "++show n
  tBegin <- getCurrentTime
  raw    <- readGeomFile n file
  tEnd   <- getCurrentTime
  putStrLn$ "Done reading (took "++show (diffUTCTime tEnd tBegin)++"), converting to Acc array.."
#endif

  tBegin <- getCurrentTime
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


#ifdef EXTRAINITCUDA
  initialise []
  putStrLn$"CUDA initialized - this is a hack to work around an apparent accelerate-cuda bug."
#endif

  performGC
  tEnd   <- getCurrentTime
  putStrLn$ "Input in CPU memory and did GC (took "++show (diffUTCTime tEnd tBegin)++"), starting benchmark..."
  tBegin <- getCurrentTime
  let simpl = phase1 $ phase0 (calcAccels input)
#ifndef NOSIMPLE
  (times,output) <- runTimedSimple Bkend.defaultBackend Nothing Bkend.defaultTrafoConfig simpl
  tEnd   <- getCurrentTime
  putStrLn$ "Finished executing through SimpleBackend. "
#else
  (times,output) <- runTimed Bkend.defaultBackend Nothing Bkend.defaultTrafoConfig (calcAccels input)
  tEnd   <- getCurrentTime
  putStrLn$ "  Result prefix(4): "++ show(P.take 3$ A.toList output)
  putStrLn$ "  Result shape "++ show(A.arrayShape output)
#endif
  let AccTiming{compileTime,runTime,copyTime} = times
  putStrLn$ "  All timing: "++ show times
  putStrLn$ "  Total time for runTimed "++ show (diffUTCTime tEnd tBegin)
  putStrLn$ "JITTIME: "++ show compileTime
  putStrLn$ "SELFTIMED: "++ show (runTime + copyTime)
#ifdef NOSIMPLE
  putStrLn$ "Writing output file to: "++ outputFile
  writeGeomFile outputFile output
#endif

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

