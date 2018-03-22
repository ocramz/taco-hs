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

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module Data.Tensor
  -- (
  -- -- * Tensor type
  -- Tensor(..),
  -- tshape, tdata, nnz, rank, dim, 
  -- -- * Shape type
  -- Sh(..),
  -- -- * Dimension types
  -- Dim.Dd(..), Dim.Sd(..)
  -- )
  where

-- import qualified Data.Vector as V
-- import qualified Data.Vector.Unboxed as VU
-- import Data.Int (Int32)
import Control.Applicative
import Control.Arrow ((&&&))
import qualified Data.Set as S
import qualified Data.Map as M
import "exceptions" Control.Monad.Catch (MonadThrow(..), throwM)
import Data.Tensor.Exception
import Data.Shape.Types (Shape(..), rank, dim, Z, (:#), (:.))
import Data.Shape.Dynamic.Named -- (Sh(..), DimE, shDiff)
import Data.Dim.Generic (Dd(..), Sd(..))


-- | Covariant indices, contravariant indices, container type, element type
data Tensor i j v e where
  Tensor ::    
       Sh n v i                    
    -> Sh n v i                    
    -> v e  
    -> Tensor (Sh n v i) (Sh n v i) v e

instance (Show i, Show j) => Show (Tensor i j v e) where
  show (Tensor shco shcontra _) =
    unwords ["covariant:", show shco,
             "contravariant:", show shcontra]

-- | Nonzeros in the tensor data
nnz :: Foldable v => Tensor i j v e -> Int
nnz (Tensor _ _ v) = length v

-- | Tensor dimensions
tdim :: (Shape i, Shape j) => Tensor i j v e -> ([Int], [Int])
tdim = dim . coIx &&& dim . contraIx

-- | Safe tensor construction; for now it only compares the length of the entry vector with the upper bound on the tensor size (i.e. considering all dimensions as dense). Can be refined by computing effective nonzeros along sparse dimensions
mkTensor :: (Integral i, Foldable v, MonadThrow m) =>
     Sh n v i -> Sh n v i -> v e -> m (Tensor (Sh n v i) (Sh n v i) v e)
mkTensor shco shcontra vdat
  | vd <= dtot = pure $ mkTensorUnsafe shco shcontra vdat
  | otherwise = throwM (IncompatDataSize vd dtot)
  where
    vd = length vdat
    dtot = product (dim shco) * product (dim shcontra)

mkTensorUnsafe ::
  Sh n v i -> Sh n v i -> v e -> Tensor (Sh n v i) (Sh n v i) v e
mkTensorUnsafe = Tensor

instance Functor v => Functor (Tensor i j v) where
  fmap f (Tensor shi shj v) = Tensor shi shj (f <$> v)

-- | Covariant indices
coIx :: Tensor i j v e -> i
coIx (Tensor ix _ _) = ix

-- | Contravariant indices
contraIx :: Tensor i j v e -> j
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







-- test data

t0 = mkTensorUnsafe tdco tdcontra [1..12]

data Ix = I | J | K | L | M | N deriving (Eq, Show, Ord, Enum)

tdco, tdcontra :: Sh Ix [] Int
tdco = mkSh $ zip [I, J ..] [Left (Dd 3), Left (Dd 2)]

tdcontra = mkSh $ zip [K] [Left (Dd 2)]



{-
NB: two tensors can be contracted over an index 'i' if this appears 
-}
  









