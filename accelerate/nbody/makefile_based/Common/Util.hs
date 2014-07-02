{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-} -- hack
module Common.Util (

  magV,
  plusV,
--  mulSV,
--  normaliseV

) where

import Data.Array.Accelerate            as A

-- | The magnitude of a vector.
--
magV :: (Elt a, IsFloating a) => Exp (a, a) -> Exp a
magV v =
  let (x, y) = unlift v
  in  sqrt (x * x + y * y)


-- | Add two vectors component-wise
--
plusV :: (Elt a, IsNum a) => Exp (a, a, a) -> Exp (a, a, a) -> Exp (a, a, a)
plusV u v = lift ( fst3 u + fst3 v
                 , snd3 u + snd3 v 
                 , thd3 u + thd3 v )

fst3 :: forall f a b c . Unlift f (f a, f b, f c) => f (Plain (f a), Plain (f b), Plain (f c)) -> f a
fst3 tup = let (a,_::f b,_::f c) = unlift tup in a 

snd3 :: forall f a b c . Unlift f (f a, f b, f c) => f (Plain (f a), Plain (f b), Plain (f c)) -> f b
snd3 tup = let (_::f a,y::f b,_::f c) = unlift tup in y

thd3 :: forall f a b c . Unlift f (f a, f b, f c) => f (Plain (f a), Plain (f b), Plain (f c)) -> f c
thd3 tup = let (_::f a,_::f b,z::f c) = unlift tup in z

{-
-- | Multiply a vector by a scalar.
--
mulSV :: (Elt a, IsNum a) => Exp a -> Exp (a, a) -> Exp (a, a)
mulSV s v = lift ( s * A.fst v
                 , s * A.fst v )


-- | Normalise a vector, so it has a magnitude of 1.
--
normaliseV :: (Elt a, IsFloating a) => Exp (a, a) -> Exp (a, a)
normaliseV v = mulSV (1 / magV v) v

-}