{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedStrings #-}
--
-- An N-Body simulation
--

module Main where

import Data.Array.Accelerate          as A
import Data.Array.Accelerate          ((:.),Z(Z))
import Data.Array.Accelerate.Trafo (convertAccWith, Phase(..))

import qualified Data.Array.Accelerate.CUDA as CUDA
import qualified Data.Array.Accelerate.Cilk as Cilk
import qualified Data.Array.Accelerate.Interpreter as I

import Data.Array.Accelerate.Multi (runMultiple)

import Data.Array.Accelerate.BackendClass (runTimed, AccTiming(..))
import Data.Array.Accelerate.BackendClass as BC

-- system
import Control.Exception (evaluate)
import Control.Monad     (forM_,when)
import Prelude                                  as P
import           Data.Char (isSpace)
import qualified Data.Array.Unboxed             as U
import qualified Data.List as L
import           Data.Time.Clock (getCurrentTime, diffUTCTime)
import           Data.ByteString.Lex.Double (readDouble)
import qualified Data.ByteString.Char8 as B
import System.Environment
import System.IO
import System.Environment (getEnvironment)
import System.Mem  (performGC)
import System.Exit 
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

main = do 
  args <- getArgs
  (n,cpu_weight,gpu_weight,which) <- 
       case args of
         []  -> do putStrLn "Using default size for input."
                   return (100,1,1, "all")
         [n] -> do putStrLn$ "NBODY size on command line: N="++ show n
                   return (read n, 1,1, "all")
         [n,c,g,which] -> return (read n, read c, read g, which)

  env <- getEnvironment
  let file = case L.lookup "ACCELERATE_INPUT_FILE" env of
              Nothing -> defaultInputFile
              Just s -> s
  putStrLn$ "NBODY: Reading requested prefix of input file... "++show n
  tBegin <- getCurrentTime
  raw    <- readGeomFile (Just n) file
  tEnd   <- getCurrentTime
  putStrLn$ "Done reading (took "++show (diffUTCTime tEnd tBegin)++"), converting to Acc array.."
  tBegin <- getCurrentTime

  let input :: A.Acc (A.Vector Position)
      input  = A.use input0
      input0 = A.fromIArray $ raw 
  putStrLn$ "  Input prefix(4) "++ show(P.take 3$ U.elems raw)
  evaluate input0
  performGC
  tEnd   <- getCurrentTime
  putStrLn$ "Input in CPU memory and did GC (took "++show (diffUTCTime tEnd tBegin)++"), starting benchmark..."
  ----------------------------------------

  let 
      total = cpu_weight + gpu_weight
      big    = ((n * gpu_weight) `quot` total)
      little = (n - big)
      prog1 = calcAccels1 (A.constant big) input
--      prog2 = calcAccels2 half input
      prog2 = calcAccels1 (A.constant little) input

      full  = calcAccels1 (A.constant n) input

  putStrLn$ "DIVIDING WORK INTO PIECES: "++show (big,little)

  let x :: (Acc Ty, BC.SomeBackend, Phase)
      x = ( prog1, BC.SomeBackend CUDA.defaultBackend, CUDA.defaultTrafoConfig) 

  let y :: (Acc Ty, BC.SomeBackend, Phase)
      y = ( prog2, BC.SomeBackend Cilk.defaultBackend, Cilk.defaultTrafoConfig)


  let gogo "all" = do gogo "cpufrac"
                      gogo "gpufrac"
                      gogo "cpufull"
                      gogo "gpufull"
                      gogo "cpugpu" 
      gogo "cpufull" = do 
        putStrLn "\n\nCPU full run:"
        runMultiple [ ( full, BC.SomeBackend Cilk.defaultBackend, Cilk.defaultTrafoConfig) ]

      gogo "gpufull" = do 
        putStrLn "\n\nGPU full run:"
        runMultiple [ ( full, BC.SomeBackend CUDA.defaultBackend, CUDA.defaultTrafoConfig ) ]

      gogo "gpufrac" = do 
        putStrLn "\n\nGPU fraction run:"
        runMultiple [ y ]

      gogo "cpufrac" = do 
        putStrLn "\n\nCPU fraction run:"
        runMultiple [ y ]

      -- Temp, test individually first:
      gogo "cpugpu" = do 
        putStrLn "\n\nNow for SPLIT run CPU/GPU:"
        runMultiple [ x, y ]
        putStrLn "All done with runMultiple!"

      gogo "gpugpu" = do 
        putStrLn "\n\nNow for SPLIT run on GPU/GPU:"
        let [b1,b2] = CUDA.allBackends
        runMultiple [ ( prog1, BC.SomeBackend b1, CUDA.defaultTrafoConfig)
                    , ( prog2, BC.SomeBackend b2, CUDA.defaultTrafoConfig) ]
        putStrLn "All done with runMultiple!"

  gogo which
  exitSuccess

-- type Ty = (Scalar Int)
type Ty = (A.Vector Accel)


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


calcAccels1 :: Exp Int -> A.Acc (A.Vector Position) -> A.Acc (A.Vector Accel)
calcAccels1 half bodies
  = let n       = A.size bodies
--        half    = n `quot` 2
        front   = A.take half bodies

        -- Replicate the little one all the way:
        cols    = A.replicate (lift $ Z :. All :. n)    front
        -- And the big one half way:
        rows    = A.replicate (lift $ Z :. half :. All) bodies

    in
    A.fold plusV (constant (0,0,0)) $ A.zipWith (accel) rows cols


-- calcAccels2 :: Exp Int -> A.Acc (A.Vector Position) -> A.Acc (A.Vector Position) -> A.Acc (A.Vector Accel)
calcAccels2 :: Exp Int -> A.Acc (A.Vector Position) -> A.Acc (A.Vector Accel)
calcAccels2 half bodies
  = let n       = A.size bodies
--        half    = n `quot` 2
        back    = A.drop half bodies

        -- Replicate the little one all the way:
        cols    = A.replicate (lift $ Z :. All :. n)    back
        -- And the big one half way:
        rows    = A.replicate (lift $ Z :. half :. All) bodies
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


{-

Prog {progBinds = [ProgBind aLt2
                            (TArray 1 (TTuple [TDouble,TDouble,TDouble]))
                            (KnownSize [10], Orig)
                            (Right Use AccArray [10] [0.204377359711,-0.124071846716,9.23475595191e-2,0.855260848999,-0.355440662242,-0.699805021286,-0.225843770429,4.79635689408e-2,-0.108046301641,0.881816339679] [0.58752346877,-0.430925352499,0.471875966527,0.17408952117,0.29258855246,-0.290639472194,-0.214849968441,0.789017047733,-0.568562230095,-0.122657088563] [0.466465813108,0.818155869842,-0.390485706739,-5.50553835928e-2,5.77864721417e-2,-0.194938545115,-0.948073166423,0.361016079783,-0.264976296574,-0.182035161182]),
                   ProgBind aLt2_0
                            (TArray 1 (TTuple [TDouble,TDouble,TDouble]))
                            (KnownSize [5], Fissioned 0 2 aLt2)
                            (Right Use AccArray [5] [0.204377359711,-0.124071846716,9.23475595191e-2,0.855260848999,-0.355440662242] [0.58752346877,-0.430925352499,0.471875966527,0.17408952117,0.29258855246] [0.466465813108,0.818155869842,-0.390485706739,-5.50553835928e-2,5.77864721417e-2]),
                   ProgBind aLt2_1
                            (TArray 1 (TTuple [TDouble,TDouble,TDouble]))
                            (KnownSize [5], Fissioned 1 2 aLt2)
                            (Right Use AccArray [5] [-0.699805021286,-0.225843770429,4.79635689408e-2,-0.108046301641,0.881816339679] [-0.290639472194,-0.214849968441,0.789017047733,-0.568562230095,-0.122657088563] [-0.194938545115,-0.948073166423,0.361016079783,-0.264976296574,-0.182035161182]),
                   ProgBind tmp_2_3
                            (TArray 2 (TTuple [TDouble,TDouble,TDouble]))
                            (KnownSize [10,5], Fissioned 0 2 tmp_2)
                            (Right Replicate [Fixed,All]
                                             (ETuple [EConst (I 10),EConst (Tup [])])
                                             aLt2_0),
                   ProgBind tmp_2_4
                            (TArray 2 (TTuple [TDouble,TDouble,TDouble]))
                            (KnownSize [10,5], Fissioned 1 2 tmp_2)
                            (Right Replicate [Fixed,All]
                                             (ETuple [EConst (I 10),EConst (Tup [])])
                                             aLt2_1),
                   ProgBind tmp_2
                            (TArray 2 (TTuple [TDouble,TDouble,TDouble]))
                            (KnownSize [10,10], Concatted [tmp_2_3,tmp_2_4])
                            (Right Generate (ETuple [EConst (I 10),EConst (I 10)])
                                            (Lam1 (ix5, TTuple [TInt,TInt])
                                                  (ECond (EPrimApp TBool
                                                                   (SP Lt)
                                                                   [ETupProject {indexFromRight = 0,
                                                                                 projlen = 1,
                                                                                 tupexpr = EVr ix5},
                                                                    EConst (I 5)])
                                                         (EIndexScalar tmp_2_3
                                                                       (ETuple [ETupProject {indexFromRight = 1,
                                                                                             projlen = 1,
                                                                                             tupexpr = EVr ix5},
                                                                                ETupProject {indexFromRight = 0,
                                                                                             projlen = 1,
                                                                                             tupexpr = EVr ix5}]))
                                                         (EIndexScalar tmp_2_4
                                                                       (ETuple [ETupProject {indexFromRight = 1,
                                                                                             projlen = 1,
                                                                                             tupexpr = EVr ix5},
                                                                                EPrimApp TInt
                                                                                         (NP Sub)
                                                                                         [ETupProject {indexFromRight = 0,
                                                                                                       projlen = 1,
                                                                                                       tupexpr = EVr ix5},
                                                                                          EConst (I 5)]]))))),
                   ProgBind tmp_3_6
                            (TArray 2 (TTuple [TDouble,TDouble,TDouble]))
                            (KnownSize [10,5], Fissioned 0 2 tmp_3)
                            (Right Replicate [All,Fixed]
                                             (ETuple [EConst (Tup []),EConst (I 10)])
                                             aLt2_0),
                   ProgBind tmp_3_7
                            (TArray 2 (TTuple [TDouble,TDouble,TDouble]))
                            (KnownSize [10,5], Fissioned 1 2 tmp_3)
                            (Right Replicate [All,Fixed]
                                             (ETuple [EConst (Tup []),EConst (I 10)])
                                             aLt2_1),
                   ProgBind tmp_3
                            (TArray 2 (TTuple [TDouble,TDouble,TDouble]))
                            (KnownSize [10,10], Concatted [tmp_3_6,tmp_3_7])
                            (Right Generate (ETuple [EConst (I 10),EConst (I 10)])
                                            (Lam1 (ix8, TTuple [TInt,TInt])
                                                  (ECond (EPrimApp TBool
                                                                   (SP Lt)
                                                                   [ETupProject {indexFromRight = 0,
                                                                                 projlen = 1,
                                                                                 tupexpr = EVr ix8},
                                                                    EConst (I 5)])
                                                         (EIndexScalar tmp_3_6
                                                                       (ETuple [ETupProject {indexFromRight = 1,
                                                                                             projlen = 1,
                                                                                             tupexpr = EVr ix8},
                                                                                ETupProject {indexFromRight = 0,
                                                                                             projlen = 1,
                                                                                             tupexpr = EVr ix8}]))
                                                         (EIndexScalar tmp_3_7
                                                                       (ETuple [ETupProject {indexFromRight = 1,
                                                                                             projlen = 1,
                                                                                             tupexpr = EVr ix8},
                                                                                EPrimApp TInt
                                                                                         (NP Sub)
                                                                                         [ETupProject {indexFromRight = 0,
                                                                                                       projlen = 1,
                                                                                                       tupexpr = EVr ix8},
                                                                                          EConst (I 5)]]))))),
                   ProgBind tmp_1_9
                            (TArray 2 (TTuple [TDouble,TDouble,TDouble]))
                            (KnownSize [10,5], Fissioned 0 2 tmp_1)
                            (Right ZipWith (Lam2 (v3, TTuple [TDouble,TDouble,TDouble])
                                                 (v4, TTuple [TDouble,TDouble,TDouble])
                                                 (ELet (e5,
                                                        TDouble,
                                                        ETupProject {indexFromRight = 1,
                                                                     projlen = 1,
                                                                     tupexpr = EVr v4})
                                                       (ELet (e6,
                                                              TDouble,
                                                              ETupProject {indexFromRight = 1,
                                                                           projlen = 1,
                                                                           tupexpr = EVr v3})
                                                             (ELet (e7,
                                                                    TDouble,
                                                                    ETupProject {indexFromRight = 0,
                                                                                 projlen = 1,
                                                                                 tupexpr = EVr v4})
                                                                   (ELet (e8,
                                                                          TDouble,
                                                                          ETupProject {indexFromRight = 0,
                                                                                       projlen = 1,
                                                                                       tupexpr = EVr v3})
                                                                         (ELet (e9,
                                                                                TDouble,
                                                                                ETupProject {indexFromRight = 2,
                                                                                             projlen = 1,
                                                                                             tupexpr = EVr v3})
                                                                               (ELet (e10,
                                                                                      TDouble,
                                                                                      ETupProject {indexFromRight = 2,
                                                                                                   projlen = 1,
                                                                                                   tupexpr = EVr v4})
                                                                                     (ECond (EPrimApp TBool
                                                                                                      (BP Not)
                                                                                                      [EPrimApp TBool
                                                                                                                (BP And)
                                                                                                                [EPrimApp TBool
                                                                                                                          (SP Eq)
                                                                                                                          [EVr e9,
                                                                                                                           EVr e10],
                                                                                                                 EPrimApp TBool
                                                                                                                          (BP And)
                                                                                                                          [EPrimApp TBool
                                                                                                                                    (SP Eq)
                                                                                                                                    [EVr e6,
                                                                                                                                     EVr e5],
                                                                                                                           EPrimApp TBool
                                                                                                                                    (SP Eq)
                                                                                                                                    [EVr e8,
                                                                                                                                     EVr e7]]]])
                                                                                            (ELet (e11,
                                                                                                   TDouble,
                                                                                                   EPrimApp TDouble
                                                                                                            (NP Sub)
                                                                                                            [EVr e7,
                                                                                                             EVr e8])
                                                                                                  (ELet (e12,
                                                                                                         TDouble,
                                                                                                         EPrimApp TDouble
                                                                                                                  (NP Sub)
                                                                                                                  [EVr e10,
                                                                                                                   EVr e9])
                                                                                                        (ELet (e13,
                                                                                                               TDouble,
                                                                                                               EPrimApp TDouble
                                                                                                                        (NP Sub)
                                                                                                                        [EVr e5,
                                                                                                                         EVr e6])
                                                                                                              (ELet (e14,
                                                                                                                     TDouble,
                                                                                                                     EPrimApp TDouble
                                                                                                                              (NP Add)
                                                                                                                              [EPrimApp TDouble
                                                                                                                                        (NP Add)
                                                                                                                                        [EPrimApp TDouble
                                                                                                                                                  (NP Mul)
                                                                                                                                                  [EVr e12,
                                                                                                                                                   EVr e12],
                                                                                                                                         EPrimApp TDouble
                                                                                                                                                  (NP Mul)
                                                                                                                                                  [EVr e13,
                                                                                                                                                   EVr e13]],
                                                                                                                               EPrimApp TDouble
                                                                                                                                        (NP Mul)
                                                                                                                                        [EVr e11,
                                                                                                                                         EVr e11]])
                                                                                                                    (ELet (e15,
                                                                                                                           TDouble,
                                                                                                                           EPrimApp TDouble
                                                                                                                                    (FP Sqrt)
                                                                                                                                    [EVr e14])
                                                                                                                          (ELet (e16,
                                                                                                                                 TDouble,
                                                                                                                                 EPrimApp TDouble
                                                                                                                                          (FP FDiv)
                                                                                                                                          [EPrimApp TDouble
                                                                                                                                                    (NP Mul)
                                                                                                                                                    [EConst (D 1.0),
                                                                                                                                                     EConst (D 1.0)],
                                                                                                                                           EVr e14])
                                                                                                                                (ETuple [EPrimApp TDouble
                                                                                                                                                  (FP FDiv)
                                                                                                                                                  [EPrimApp TDouble
                                                                                                                                                            (NP Mul)
                                                                                                                                                            [EVr e16,
                                                                                                                                                             EVr e12],
                                                                                                                                                   EVr e15],
                                                                                                                                         EPrimApp TDouble
                                                                                                                                                  (FP FDiv)
                                                                                                                                                  [EPrimApp TDouble
                                                                                                                                                            (NP Mul)
                                                                                                                                                            [EVr e16,
                                                                                                                                                             EVr e13],
                                                                                                                                                   EVr e15],
                                                                                                                                         EPrimApp TDouble
                                                                                                                                                  (FP FDiv)
                                                                                                                                                  [EPrimApp TDouble
                                                                                                                                                            (NP Mul)
                                                                                                                                                            [EVr e16,
                                                                                                                                                             EVr e11],
                                                                                                                                                   EVr e15]])))))))
                                                                                            (ETuple [EConst (D 0.0),
                                                                                                     EConst (D 0.0),
                                                                                                     EConst (D 0.0)])))))))))
                                           tmp_2_3
                                           tmp_3_6),
                   ProgBind tmp_1_10
                            (TArray 2 (TTuple [TDouble,TDouble,TDouble]))
                            (KnownSize [10,5], Fissioned 1 2 tmp_1)
                            (Right ZipWith (Lam2 (v3, TTuple [TDouble,TDouble,TDouble])
                                                 (v4, TTuple [TDouble,TDouble,TDouble])
                                                 (ELet (e5,
                                                        TDouble,
                                                        ETupProject {indexFromRight = 1,
                                                                     projlen = 1,
                                                                     tupexpr = EVr v4})
                                                       (ELet (e6,
                                                              TDouble,
                                                              ETupProject {indexFromRight = 1,
                                                                           projlen = 1,
                                                                           tupexpr = EVr v3})
                                                             (ELet (e7,
                                                                    TDouble,
                                                                    ETupProject {indexFromRight = 0,
                                                                                 projlen = 1,
                                                                                 tupexpr = EVr v4})
                                                                   (ELet (e8,
                                                                          TDouble,
                                                                          ETupProject {indexFromRight = 0,
                                                                                       projlen = 1,
                                                                                       tupexpr = EVr v3})
                                                                         (ELet (e9,
                                                                                TDouble,
                                                                                ETupProject {indexFromRight = 2,
                                                                                             projlen = 1,
                                                                                             tupexpr = EVr v3})
                                                                               (ELet (e10,
                                                                                      TDouble,
                                                                                      ETupProject {indexFromRight = 2,
                                                                                                   projlen = 1,
                                                                                                   tupexpr = EVr v4})
                                                                                     (ECond (EPrimApp TBool
                                                                                                      (BP Not)
                                                                                                      [EPrimApp TBool
                                                                                                                (BP And)
                                                                                                                [EPrimApp TBool
                                                                                                                          (SP Eq)
                                                                                                                          [EVr e9,
                                                                                                                           EVr e10],
                                                                                                                 EPrimApp TBool
                                                                                                                          (BP And)
                                                                                                                          [EPrimApp TBool
                                                                                                                                    (SP Eq)
                                                                                                                                    [EVr e6,
                                                                                                                                     EVr e5],
                                                                                                                           EPrimApp TBool
                                                                                                                                    (SP Eq)
                                                                                                                                    [EVr e8,
                                                                                                                                     EVr e7]]]])
                                                                                            (ELet (e11,
                                                                                                   TDouble,
                                                                                                   EPrimApp TDouble
                                                                                                            (NP Sub)
                                                                                                            [EVr e7,
                                                                                                             EVr e8])
                                                                                                  (ELet (e12,
                                                                                                         TDouble,
                                                                                                         EPrimApp TDouble
                                                                                                                  (NP Sub)
                                                                                                                  [EVr e10,
                                                                                                                   EVr e9])
                                                                                                        (ELet (e13,
                                                                                                               TDouble,
                                                                                                               EPrimApp TDouble
                                                                                                                        (NP Sub)
                                                                                                                        [EVr e5,
                                                                                                                         EVr e6])
                                                                                                              (ELet (e14,
                                                                                                                     TDouble,
                                                                                                                     EPrimApp TDouble
                                                                                                                              (NP Add)
                                                                                                                              [EPrimApp TDouble
                                                                                                                                        (NP Add)
                                                                                                                                        [EPrimApp TDouble
                                                                                                                                                  (NP Mul)
                                                                                                                                                  [EVr e12,
                                                                                                                                                   EVr e12],
                                                                                                                                         EPrimApp TDouble
                                                                                                                                                  (NP Mul)
                                                                                                                                                  [EVr e13,
                                                                                                                                                   EVr e13]],
                                                                                                                               EPrimApp TDouble
                                                                                                                                        (NP Mul)
                                                                                                                                        [EVr e11,
                                                                                                                                         EVr e11]])
                                                                                                                    (ELet (e15,
                                                                                                                           TDouble,
                                                                                                                           EPrimApp TDouble
                                                                                                                                    (FP Sqrt)
                                                                                                                                    [EVr e14])
                                                                                                                          (ELet (e16,
                                                                                                                                 TDouble,
                                                                                                                                 EPrimApp TDouble
                                                                                                                                          (FP FDiv)
                                                                                                                                          [EPrimApp TDouble
                                                                                                                                                    (NP Mul)
                                                                                                                                                    [EConst (D 1.0),
                                                                                                                                                     EConst (D 1.0)],
                                                                                                                                           EVr e14])
                                                                                                                                (ETuple [EPrimApp TDouble
                                                                                                                                                  (FP FDiv)
                                                                                                                                                  [EPrimApp TDouble
                                                                                                                                                            (NP Mul)
                                                                                                                                                            [EVr e16,
                                                                                                                                                             EVr e12],
                                                                                                                                                   EVr e15],
                                                                                                                                         EPrimApp TDouble
                                                                                                                                                  (FP FDiv)
                                                                                                                                                  [EPrimApp TDouble
                                                                                                                                                            (NP Mul)
                                                                                                                                                            [EVr e16,
                                                                                                                                                             EVr e13],
                                                                                                                                                   EVr e15],
                                                                                                                                         EPrimApp TDouble
                                                                                                                                                  (FP FDiv)
                                                                                                                                                  [EPrimApp TDouble
                                                                                                                                                            (NP Mul)
                                                                                                                                                            [EVr e16,
                                                                                                                                                             EVr e11],
                                                                                                                                                   EVr e15]])))))))
                                                                                            (ETuple [EConst (D 0.0),
                                                                                                     EConst (D 0.0),
                                                                                                     EConst (D 0.0)])))))))))
                                           tmp_2_4
                                           tmp_3_7),
                   ProgBind tmp_1
                            (TArray 2 (TTuple [TDouble,TDouble,TDouble]))
                            (KnownSize [10,10], Concatted [tmp_1_9,tmp_1_10])
                            (Right Generate (ETuple [EConst (I 10),EConst (I 10)])
                                            (Lam1 (ix11, TTuple [TInt,TInt])
                                                  (ECond (EPrimApp TBool
                                                                   (SP Lt)
                                                                   [ETupProject {indexFromRight = 0,
                                                                                 projlen = 1,
                                                                                 tupexpr = EVr ix11},
                                                                    EConst (I 5)])
                                                         (EIndexScalar tmp_1_9
                                                                       (ETuple [ETupProject {indexFromRight = 1,
                                                                                             projlen = 1,
                                                                                             tupexpr = EVr ix11},
                                                                                ETupProject {indexFromRight = 0,
                                                                                             projlen = 1,
                                                                                             tupexpr = EVr ix11}]))
                                                         (EIndexScalar tmp_1_10
                                                                       (ETuple [ETupProject {indexFromRight = 1,
                                                                                             projlen = 1,
                                                                                             tupexpr = EVr ix11},
                                                                                EPrimApp TInt
                                                                                         (NP Sub)
                                                                                         [ETupProject {indexFromRight = 0,
                                                                                                       projlen = 1,
                                                                                                       tupexpr = EVr ix11},
                                                                                          EConst (I 5)]]))))),
                   ProgBind tmp_0_12
                            (TArray 1 (TTuple [TDouble,TDouble,TDouble]))
                            (KnownSize [5], Fissioned 0 2 tmp_0)
                            (Right Fold (Lam2 (v0, TTuple [TDouble,TDouble,TDouble])
                                              (v1, TTuple [TDouble,TDouble,TDouble])
                                              (ETuple [EPrimApp TDouble
                                                                (NP Add)
                                                                [ETupProject {indexFromRight = 2,
                                                                              projlen = 1,
                                                                              tupexpr = EVr v0},
                                                                 ETupProject {indexFromRight = 2,
                                                                              projlen = 1,
                                                                              tupexpr = EVr v1}],
                                                       EPrimApp TDouble
                                                                (NP Add)
                                                                [ETupProject {indexFromRight = 1,
                                                                              projlen = 1,
                                                                              tupexpr = EVr v0},
                                                                 ETupProject {indexFromRight = 1,
                                                                              projlen = 1,
                                                                              tupexpr = EVr v1}],
                                                       EPrimApp TDouble
                                                                (NP Add)
                                                                [ETupProject {indexFromRight = 0,
                                                                              projlen = 1,
                                                                              tupexpr = EVr v0},
                                                                 ETupProject {indexFromRight = 0,
                                                                              projlen = 1,
                                                                              tupexpr = EVr v1}]]))
                                        (EConst (Tup [D 0.0,D 0.0,D 0.0]))
                                        tmp_1_9),
                   ProgBind tmp_0_13
                            (TArray 1 (TTuple [TDouble,TDouble,TDouble]))
                            (KnownSize [5], Fissioned 1 2 tmp_0)
                            (Right Fold (Lam2 (v0, TTuple [TDouble,TDouble,TDouble])
                                              (v1, TTuple [TDouble,TDouble,TDouble])
                                              (ETuple [EPrimApp TDouble
                                                                (NP Add)
                                                                [ETupProject {indexFromRight = 2,
                                                                              projlen = 1,
                                                                              tupexpr = EVr v0},
                                                                 ETupProject {indexFromRight = 2,
                                                                              projlen = 1,
                                                                              tupexpr = EVr v1}],
                                                       EPrimApp TDouble
                                                                (NP Add)
                                                                [ETupProject {indexFromRight = 1,
                                                                              projlen = 1,
                                                                              tupexpr = EVr v0},
                                                                 ETupProject {indexFromRight = 1,
                                                                              projlen = 1,
                                                                              tupexpr = EVr v1}],
                                                       EPrimApp TDouble
                                                                (NP Add)
                                                                [ETupProject {indexFromRight = 0,
                                                                              projlen = 1,
                                                                              tupexpr = EVr v0},
                                                                 ETupProject {indexFromRight = 0,
                                                                              projlen = 1,
                                                                              tupexpr = EVr v1}]]))
                                        (EConst (Tup [D 0.0,D 0.0,D 0.0]))
                                        tmp_1_10),
                   ProgBind tmp_0
                            (TArray 1 (TTuple [TDouble,TDouble,TDouble]))
                            (KnownSize [10], Concatted [tmp_0_12,tmp_0_13])
                            (Right Generate (EConst (I 10))
                                            (Lam1 (ix14, TInt)
                                                  (ECond (EPrimApp TBool
                                                                   (SP Lt)
                                                                   [EVr ix14,EConst (I 5)])
                                                         (EIndexScalar tmp_0_12 (EVr ix14))
                                                         (EIndexScalar tmp_0_13
                                                                       (EPrimApp TInt
                                                                                 (NP Sub)
                                                                                 [EVr ix14,
                                                                                  EConst (I 5)]))))),
                   ProgBind tmp_0_shape
                            TInt
                            (UnknownSize, Orig)
                            (Left EConst (I 10))],
      progResults = WithShapes [(tmp_0, tmp_0_shape)],
      progType = TArray 1 (TTuple [TDouble,TDouble,TDouble]),
      uniqueCounter = 15,
      typeEnv = [(aLt2, TArray 1 (TTuple [TDouble,TDouble,TDouble])),
                 (aLt2_0, TArray 1 (TTuple [TDouble,TDouble,TDouble])),
                 (aLt2_1, TArray 1 (TTuple [TDouble,TDouble,TDouble])),
                 (tmp_0, TArray 1 (TTuple [TDouble,TDouble,TDouble])),
                 (tmp_0_12, TArray 1 (TTuple [TDouble,TDouble,TDouble])),
                 (tmp_0_13, TArray 1 (TTuple [TDouble,TDouble,TDouble])),
                 (tmp_0_shape, TInt),
                 (tmp_1, TArray 2 (TTuple [TDouble,TDouble,TDouble])),
                 (tmp_1_10, TArray 2 (TTuple [TDouble,TDouble,TDouble])),
                 (tmp_1_9, TArray 2 (TTuple [TDouble,TDouble,TDouble])),
                 (tmp_2, TArray 2 (TTuple [TDouble,TDouble,TDouble])),
                 (tmp_2_3, TArray 2 (TTuple [TDouble,TDouble,TDouble])),
                 (tmp_2_4, TArray 2 (TTuple [TDouble,TDouble,TDouble])),
                 (tmp_3, TArray 2 (TTuple [TDouble,TDouble,TDouble])),
                 (tmp_3_6, TArray 2 (TTuple [TDouble,TDouble,TDouble])),
                 (tmp_3_7, TArray 2 (TTuple [TDouble,TDouble,TDouble]))]}

-}


