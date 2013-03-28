{-# LANGUAGE FlexibleContexts, CPP #-}

module Main where

import Random
import SMVM.Matrix

import System.Random.MWC
import Data.Array.Unboxed
import Data.Array.Accelerate           (Vector, Segments, Acc)

#ifdef ACCBACKEND
import qualified ACCBACKEND as Bkend
#else
import qualified Data.Array.Accelerate.CUDA as Bkend
#endif

import           Data.Time.Clock (getCurrentTime, diffUTCTime)

import qualified Data.Array.Accelerate as Acc
import qualified Data.Vector.Unboxed   as V


import Control.Exception (evaluate)
import System.Environment (getArgs)

-- Sparse-matrix vector multiplication
-- -----------------------------------

type SparseVector a = (Vector Int, Vector a)
type SparseMatrix a = (Segments Int, SparseVector a)

smvmAcc :: SparseMatrix Float -> Vector Float -> Acc (Vector Float)
smvmAcc (segd', (inds', vals')) vec'
  = let
      segd     = Acc.use segd'
      inds     = Acc.use inds'
      vals     = Acc.use vals'
      vec      = Acc.use vec'
      ---
      vecVals  = Acc.backpermute (Acc.shape inds) (\i -> Acc.index1 $ inds Acc.! i) vec
      products = Acc.zipWith (*) vecVals vals
    in
    Acc.foldSeg (+) 0 products segd


-- The reference version will be slow, with many conversions between
-- array/vector/list representations. This will likely skew heap usage
-- calculations, but oh well...
--
type USparseMatrix a = (UArray Int Int, (UArray Int Int, UArray Int a))


-- Main
-- ----

run :: Int -> IO (() -> Acc (Vector Float))
run i = withSystemRandom $ \gen ->  do
  -- sparse-matrix
  (segd', smat') <- randomCSRMatrix gen i i
  let (ind',val') = V.unzip smat'

  segd <- convertVector segd'
  ind  <- convertVector ind'
  val  <- convertVector val'
  let smat = (segd, (ind,val))

  -- vector
  vec' <- uniformVector gen (V.length segd') :: IO (V.Vector Float)
  vec  <- convertVector vec'

  -- multiply!
  return $ run_acc smat vec
  where
    run_acc smat vec () = smvmAcc smat vec
    --
    v2a :: (V.Unbox a, IArray UArray a) => V.Vector a -> UArray Int a
    v2a vec = listArray (0, V.length vec - 1) $ V.toList vec

main :: IO ()
main = do
  args <- getArgs
  let 
    inputSize = case args of
                  [] -> 100
                  [sz] -> read sz
  run_acc <- run inputSize
  
  t1 <- getCurrentTime
  evaluate $ Bkend.run $ run_acc ()
  t2 <- getCurrentTime
  let dt = t2 `diffUTCTime` t1
  putStrLn $ "SELFTIMED-with-compile: "++ show dt

