{-# language GADTs #-}
{-# language DeriveFunctor #-}
{-# language TypeOperators #-}
{-# language PackageImports #-}
module Data.Tensor (
  -- * Tensor type
  Tensor(..),
  shape, nnz, rank, dim, 
  -- * Shape type
  Sh(..),
  -- * Dimension types
  Dim.Dd(..), Dim.Sd(..)) where

import qualified Data.Vector as V
-- import qualified Data.Vector.Unboxed as VU

-- import Data.Word (Word32, Word64)
-- import Data.Int (Int32)

import Control.Exception (Exception(..))
import "exceptions" Control.Monad.Catch (MonadThrow(..), throwM, MonadCatch(..), catch)

import qualified Data.Shape as Shape (dim, rank)
import Data.Shape (Sh(..), 
                   Z,
                   D1, D2, CSR, COO, mkD2, mkCSR, mkCOO) 
import qualified Data.Dim as Dim



{- |
IN: Tensor reduction syntax (Einstein notation)

OUT: stride program (how to read/write memory)


taco compiles a tensor expression (e.g. C = A_{ijk}B_{k} ) into a series of nested loops.

dimensions : can be either dense or sparse

internally, tensor data is stored in /dense/ vectors

"contract A_{ijk}B_{k} over the third index"

-}



-- | A generic tensor type, polymorphic in the container type as well
data GTensor c i a where
  GTensor :: Sh i -> c a -> GTensor c (Sh i) a
  
mkGT :: Sh i -> c a -> GTensor c (Sh i) a
mkGT = GTensor


-- | The 'Tensor' type. Tensor data entries are stored as one single array
data Tensor i a where
  T :: Sh i -> V.Vector a -> Tensor (Sh i) a 

-- | Construct a tensor given a shape and a vector of entries
mkT :: Sh i -> V.Vector a -> Tensor (Sh i) a
mkT = T

instance Functor (Tensor i) where
  fmap f (T sh v) = T sh (f <$> v)

-- liftA2' :: (a -> a -> b) -> Tensor i a -> Tensor i a -> Tensor i a 
-- liftA2' f (T sh1 v1) (T sh2 v2) = mkT sh1 (V.zipWith f v1 v2)

pure' :: a -> Tensor (Sh Z) a
pure' = mkT Z . V.singleton

instance (Eq a) => Eq (Tensor i a) where
  (T sh1 d1) == (T sh2 d2) = sh1 == sh2 && d1 == d2

instance (Show a) => Show (Tensor i a) where
  show (T sh d) = unwords [show sh, show $ V.take 5 d, "..."]


 

-- | Access the shape of a tensor
shape :: Tensor sh a -> sh
shape (T sh _) = sh

-- | Number of nonzero tensor elements
nnz :: Tensor i a -> Int
nnz (T _ td) = V.length td

-- | Tensor rank
rank :: Tensor i a -> Int
rank (T sh _) = Shape.rank sh

-- | Tensor dimensions
dim :: Tensor i a -> [Int]
dim (T sh _) = fromIntegral <$> Shape.dim sh

















