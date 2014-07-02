{-# LANGUAGE TypeOperators #-}

module MMult (mmul)  where 

import Data.Array.Accelerate as A

mmul :: (IsNum a, Elt a)
     => Acc (Array ((Z :. Int) :. Int) a)
     -> Acc (Array ((DIM0 :. Int) :. Int) a)
     -> Acc (Array ((Z :. Int) :. Int) a)
mmul a b = sum' (prod aCube bCube)
  where
    sum' = A.fold (+) 0
    prod = A.zipWith (*)
    t = A.transpose b
    -- What does this mean, and is it correct?
    getRow = indexHead . indexTail
    getCol = indexHead
    rowsA = getRow (A.shape a)
    colsB = getCol (A.shape b)
    sliceA = lift (Z :. All :. colsB :. All )
    sliceB = lift (Z :. rowsA :. All :. All )
    aCube = A.replicate sliceA a
    bCube = A.replicate sliceB t 
