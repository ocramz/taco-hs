{-# language GADTs, TypeOperators, DataKinds, KindSignatures #-}
module Data.Shape.Static where

import           GHC.TypeLits (Nat)

-- from https://hackage.haskell.org/package/dimensions-0.3.2.0/docs/src/Numeric-Dimensions-Idx.html#Idx
data Idx (ds :: [Nat]) where
   -- | Zero-rank dimensionality - scalar
   Zi :: Idx '[]
   -- | List-like concatenation of indices
   (:!) :: {-# UNPACK #-} !Int -> !(Idx ds) -> Idx (d ': ds)
