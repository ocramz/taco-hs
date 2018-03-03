{-# language GADTs, TypeOperators, DataKinds, KindSignatures #-}
{-# language FlexibleInstances, RankNTypes #-}
module Data.Shape.Static where

import GHC.TypeLits (Nat)
import Unsafe.Coerce (unsafeCoerce)

-- from https://hackage.haskell.org/package/dimensions-0.3.2.0/docs/src/Numeric-Dimensions-Idx.html#Idx
data Idx (ds :: [Nat]) where
   -- | Zero-rank dimensionality - scalar
   Z :: Idx '[]
   -- | List-like concatenation of indices
   (:!) :: {-# UNPACK #-} !Int -> !(Idx ds) -> Idx (d ': ds)

infixr 5 :!

idxToList :: Idx ds -> [Int]
idxToList Z = []
idxToList (x :! xs) = x : idxToList xs

-- | UNSAFE coerce 
idxFromList :: [Int] -> Idx ds
idxFromList xss = unsafeCoerce $ go xss
  where
    go [] = unsafeCoerce Z
    go (x:xs) = x :! unsafeCoerce (idxFromList xs)



instance Show (Idx ds) where
    show Z  = "Idx Ø"
    show xs = "Idx" ++ foldr (\i s -> " " ++ show i ++ s) "" (idxToList xs)

instance Eq (Idx ds) where
    Z == Z = True
    (a:!as) == (b:!bs) = a == b && as == bs
    Z /= Z = False
    (a:!as) /= (b:!bs) = a /= b || as /= bs


-- | With this instance we can slightly reduce indexing expressions
--   e.g. x ! (1 :! 2 :! 4) == x ! (1 :! 2 :! 4 :! Z)
instance Num (Idx '[n]) where
    (a:!Z) + (b:!Z) = (a+b) :! Z
    (a:!Z) - (b:!Z) = (a-b) :! Z
    (a:!Z) * (b:!Z) = (a*b) :! Z
    signum (a:!Z)   = signum a :! Z
    abs (a:!Z)      = abs a :! Z
    fromInteger i   = fromInteger i :! Z

instance Ord (Idx ds) where
    compare Z Z             = EQ
    compare (a:!as) (b:!bs) = compare as bs `mappend` compare a b

-- instance Dimensions ds => Bounded (Idx ds) where
--     maxBound = f (dim @ds)
--       where
--         f :: forall ns . Dim ns -> Idx ns
--         f D                     = Z
--         f ((Dn :: Dim n) :* ds) = dimVal' @n :! f ds
--     {-# INLINE maxBound #-}
--     minBound = f (dim @ds)
--       where
--         f :: forall (ns :: [Nat]) . Dim ns -> Idx ns
--         f D          = Z
--         f (Dn :* ds) = 1 :! f ds
--     {-# INLINE minBound #-}    
