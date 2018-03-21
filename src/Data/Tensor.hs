{-# language GADTs #-}
{-# language DeriveFunctor #-}
{-# language TypeOperators #-}
{-# language PackageImports #-}
{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}
module Data.Tensor (
  -- * Tensor type
  Tensor(..),
  tshape, tdata, nnz, rank, dim, 
  -- * Shape type
  Sh(..),
  -- * Dimension types
  Dim.Dd(..), Dim.Sd(..)) where

import qualified Data.Vector as V
-- import qualified Data.Vector.Unboxed as VU

-- import Data.Int (Int32)

import Control.Applicative


import qualified Data.Shape as Shape (Shape(..), dim, rank)
import Data.Shape.Types (Z, (:#), (:.))
import Data.Shape (Sh(..), 
                   D1, D2, CSR, COO, mkD2, mkCSR, mkCOO)
import qualified Data.Shape.Dynamic as ShDyn       
import qualified Data.Dim as Dim



-- | A tensor type with dimensions only known at runtime
data Tenzor i a = Tenzor {tzShape :: ShDyn.ShD i, tzData :: V.Vector a }

instance Integral i => Shape.Shape (Tenzor i a) where
  type ShapeT (Tenzor i a) = ShDyn.ShD i
  shape = tzShape 
  shRank = ShDyn.rank . Shape.shape
  shDim = ShDyn.dim . Shape.shape

instance Shape.Shape (Tensor (Sh i) a) where
  type ShapeT (Tensor (Sh i) a) = Sh i
  shape = tshape 
  shRank = rank
  shDim = dim






-- | The 'Tensor' type with statically known shape. Tensor data entries are stored as one single array
data Tensor i a where
  Tensor :: Sh i -> V.Vector a -> Tensor (Sh i) a 

-- | Construct a tensor given a shape and a vector of entries
mkT :: Sh i -> V.Vector a -> Tensor (Sh i) a
mkT = Tensor

instance Functor (Tensor i) where
  fmap f (Tensor sh v) = Tensor sh (f <$> v)

-- liftA2' :: (a -> a -> b) -> Tensor i a -> Tensor i a -> Tensor i a 
-- liftA2' f (T sh1 v1) (T sh2 v2) = mkT sh1 (V.zipWith f v1 v2)

pure' :: a -> Tensor (Sh Z) a
pure' = mkT Z . V.singleton

instance (Eq a) => Eq (Tensor i a) where
  (Tensor sh1 d1) == (Tensor sh2 d2) = sh1 == sh2 && d1 == d2

instance (Show a) => Show (Tensor i a) where
  show (Tensor sh d) = unwords [show sh, show $ V.take 5 d, "..."]


 

-- | Access the shape of a 'Tensor'
tshape :: Tensor sh a -> sh
tshape (Tensor sh _) = sh

-- | Access the raw data of a 'Tensor'
tdata :: Tensor sh a -> V.Vector a
tdata (Tensor _ td) = td

-- | Number of nonzero tensor elements
nnz :: Tensor i a -> Int
nnz (Tensor _ td) = V.length td

-- | Tensor rank
rank :: Tensor i a -> Int
rank (Tensor sh _) = Shape.rank sh

-- | Tensor dimensions
dim :: Tensor i a -> [Int]
dim (Tensor sh _) = Shape.dim sh















-- | playground, for future use

-- -- | A generic tensor type, polymorphic in the container type as well
-- data GTensor c i a where
--   GTensor :: Sh i -> c a -> GTensor c (Sh i) a
  
-- mkGT :: Sh i -> c a -> GTensor c (Sh i) a
-- mkGT = GTensor









