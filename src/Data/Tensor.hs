{-# language GADTs #-}
-- {-# language DeriveFunctor #-}
{-# language TypeOperators #-}
{-# language PackageImports #-}
{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}
-- {-# language RankNTypes #-}
{-|
Module      : Data.Tensor
Description : Tensor data structure
Copyright   : (c) Marco Zocca, 2018
License     : GPL-3
Maintainer  : zocca.marco gmail
Stability   : experimental
Portability : POSIX

This module exports the main tensor- and shape-related data types and functions.

A 'Tensor' is parametrized by its covariant and contravariant index sets and by an array of nonzero data. Each index can be either dense or sparse; dense tensor indices only carry a scalar dimensionality field, whereas sparse fields require a cumulative array, an index array and the dimensionality.

-}
module Data.Tensor
  (
  -- * Tensor 
  Tensor(..),
  -- ** Constructors
  mkTensor, mkTensorUnsafe,
  -- -- *** Accessors
  -- coIx, contraIx,
  -- ** Properties
  nnz, tdim, trank,
  -- ** Tensor operations
  contractionIndices, outerProdIndices, 
  -- * Shape 
  Sh(..), mkSh, Shape(..),
  -- * Dimension 
  Dd(..), Sd(..), DimE
  )
  where

-- import qualified Data.Vector as V
-- import qualified Data.Vector.Unboxed as VU
-- import Data.Int (Int32)
import Control.Applicative
import Control.Arrow ((&&&))
import qualified Data.Set as S
import qualified Data.Map as M
import "exceptions" Control.Monad.Catch (MonadThrow(..), throwM)
import Control.Exception
import Data.Tensor.Exception
import Data.Shape.Types (Shape(..), rank, dim, Z, (:#), (:.))
import Data.Shape (Sh(..), mkSh, mkShD, DimE, shDiff)
import Data.Dim.Generic (Dd(..), Sd(..))


-- -- | Covariant indices, contravariant indices, container type, element type
-- data Tensor i j v e where
--   Tensor :: 
--        Sh n v i                    
--     -> Sh n v i  
--     -> v e  
--     -> Tensor (Sh n v i) (Sh n v i) v e

data Tensor i j v e where
  Tensor ::
    (Shape i, Shape j) =>
         i
      -> j
      -> v e
      -> Tensor i j v e

instance (Show i, Show j) => Show (Tensor i j v e) where
  show (Tensor shco shcontra _) =
    unwords ["covariant:", show shco,
             "contravariant:", show shcontra]

-- | Number of nonzero entries in the tensor data
nnz :: Foldable v => Tensor i j v e -> Int
nnz (Tensor _ _ v) = length v

-- | Nonzero density
density :: (Shape i, Shape j, Foldable v, Fractional a) => Tensor i j v e -> a
density t = fromIntegral (nnz t) / fromIntegral (maxNElems t)

-- | Maximum number of elements
maxNElems :: (Shape j, Shape i) => Tensor i j v e -> Int
maxNElems t = Prelude.product pco * Prelude.product pcontra where
  (pco, pcontra) = tdim t

-- | Tensor dimensions: (covariant, contravariant)
tdim :: (Shape i, Shape j) => Tensor i j v e -> ([Int], [Int])
tdim = dim . coIx &&& dim . contraIx

-- | Tensor rank: (covariant, contravariant)
trank :: (Shape i, Shape j) => Tensor i j v e -> (Int, Int)
trank t = (length co, length contra) where
  (co, contra) = tdim t

-- | Safe tensor construction; for now it only compares the length of the entry vector with the upper bound on the tensor size (i.e. considering all dimensions as dense). Can be refined by computing effective nonzeros along sparse dimensions
mkTensor :: (Integral i, Foldable v, MonadThrow m) =>
     Sh n v i -> Sh n v i -> v e -> m (Tensor (Sh n v i) (Sh n v i) v e)
mkTensor shco shcontra vdat
  | vd <= dtot = pure $ mkTensorUnsafe shco shcontra vdat
  | otherwise = throwM (IncompatDataSize vd dtot)
  where
    vd = length vdat
    dtot = product (dim shco) * product (dim shcontra)

-- | Unsafe tensor construction. Doesn't check data size compatibility
mkTensorUnsafe :: Integral i => 
  Sh n v i -> Sh n v i -> v e -> Tensor (Sh n v i) (Sh n v i) v e
mkTensorUnsafe = Tensor

instance Functor v => Functor (Tensor i j v) where
  fmap f (Tensor shi shj v) = Tensor shi shj (f <$> v)

-- | Covariant indices
coIx :: Tensor co contra v e -> co
coIx (Tensor ix _ _) = ix

-- | Contravariant indices
contraIx :: Tensor co contra v e -> contra
contraIx (Tensor _ ix _) = ix

-- | Two tensors can be contracted if some covariant indices in the first appear in the contravariant indices of the second.
contractionIndices
  :: (Ord k, MonadThrow m, Integral i1) =>
     (DimE v1 i1 -> DimE v1 i1 -> b)   -- ^ Index combining function
     -> Tensor (Sh k v1 i1) j v2 e1
     -> Tensor i2 (Sh k v1 i1) v3 e2
     -> m (M.Map k b)
contractionIndices f t1 t2 = shDiff f (coIx t1) (contraIx t2)

-- | The outer product of two tensors is defined over the non-empty intersection of the contravariant indices of the first with the covariant ones of the second.
outerProdIndices
  :: (Ord k, MonadThrow m, Integral i1) =>
     (DimE v1 i1 -> DimE v1 i1 -> b)   -- ^ Index combining function
     -> Tensor i2 (Sh k v1 i1) v2 e1
     -> Tensor (Sh k v1 i1) j v3 e2
     -> m (M.Map k b)
outerProdIndices f t1 t2 = shDiff f (contraIx t1) (coIx t2)




-- class FromList e where
--   fromList :: [e] -> Tensor i j [] e
  








{-
NB: two tensors can be contracted over an index 'i' if this appears 
-}
  









