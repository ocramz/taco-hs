{-# language LambdaCase #-}
{-|
Module      : Data.Dim
Description : Dimension data
Copyright   : (c) Marco Zocca, 2018
License     : GPL-3
Maintainer  : ocramz fripost org
Stability   : experimental
Portability : POSIX

This module contains types and construction/access functions for tensor dimension metadata which are declared to be polymorphic in the container type (i.e. could be lists, vectors etc.).
Note : no rank or dimensionality information is known at compile time, that is, size mismatch errors will have to be raised at runtime.
-}
module Data.Dim (
  -- * Variance annotation
    Variance(..), coIx, contraIx
  -- ** Convenience constructors
  , mkVarVector, mkVarCoVector, mkVarMatrix    
  -- * Dimension metadata
  , DimE(..), dimE, denseDimE, sparseDimE
  , Dd(..), Sd(..)

  ) where

import Data.Int (Int32(..), Int64(..))
import Data.List.NonEmpty (NonEmpty(..), fromList, toList)
import qualified Data.IntMap as IM

import Data.Shape.Types


-- | Variance annotation
data Variance v i =
    CoVar (NonEmpty (DimE v i)) -- ^ Only covariant indices
  | ContraVar (NonEmpty (DimE v i)) -- ^ Only contravariant indices
  | BothVar (NonEmpty (DimE v i)) (NonEmpty (DimE v i)) -- ^ Both variant and contravariant indices
  deriving (Eq, Show)

-- | Get covariant indices
coIx :: Variance v i -> Maybe (NonEmpty (DimE v i))
coIx = \case
  CoVar ne -> Just ne
  BothVar ne _ -> Just ne
  _ -> Nothing

-- | Get contravariant indices
contraIx :: Variance v i -> Maybe (NonEmpty (DimE v i))
contraIx = \case
  BothVar _ ne -> Just ne
  ContraVar ne -> Just ne
  _ -> Nothing


-- | A vector has a single contravariant index
mkVarVector :: DimE v i -> Variance v i
mkVarVector ixco = ContraVar (fromList [ixco])
-- | A co-vector has a single covariant index
mkVarCoVector :: DimE v i -> Variance v i
mkVarCoVector ixcontra = CoVar (fromList [ixcontra])
-- | A matrix has one covariant and one contravariant index
mkVarMatrix :: DimE v i -> DimE v i -> Variance v i
mkVarMatrix ixco ixcontra = BothVar (fromList [ixco]) (fromList [ixcontra])

instance Integral i => TShape (Variance v i) where
  tdim sh = case sh of
    CoVar ne -> (toDims ne, [])
    ContraVar ne -> ([], toDims ne)
    BothVar neco necontra -> (toDims neco, toDims necontra)

toDims :: Integral i => NonEmpty (DimE v i) -> [Int]
toDims ne = (fromIntegral . dimE) `map` toList ne

-- | Contraction indices
newtype CIx = CIx (NonEmpty Int) deriving (Eq, Show)

  
-- | Tensor dimensions can be either dense or sparse
newtype DimE v i = DimE {
  unDimE :: Either (Dd i) (Sd v i)
  } deriving (Eq)

dimE :: DimE v i -> i
dimE (DimE ei) = either dDim sDim ei

instance Show i => Show (DimE v i) where
  show (DimE ei) = either shd shs ei where
    shd (Dd n) = unwords ["D", show n]
    shs (Sd _ _ n) = unwords ["S", show n]

-- | Construct a dense DimE
denseDimE :: i -> DimE v i
denseDimE = DimE . Left . Dd 

-- | Construct a sparse DimE
sparseDimE :: Maybe (v i) -> v i -> i -> DimE v i
sparseDimE sv ixv n = DimE (Right (Sd sv ixv n))


-- | To define a /dense/ dimension we only need the dimensionality parameter (an integer)
newtype Dd i = Dd {
   -- | Dimensionality
    dDim :: i
  } deriving (Eq)

-- | To define a /sparse/ dimension we need a cumulative array, an index array and a dimensionality parameter
data Sd v i = Sd {
      -- | Location in the sIdx array where each segment begins. Not all storage formats (e.g. COO for rank-2 tensors) need this information, hence it's wrapped in a Maybe.
      sPtr :: Maybe (v i)
      -- | Index array (indices of nonzero entries)
    , sIdx :: v i
      -- | Dimensionality 
    , sDim :: i
    } deriving (Eq)

instance Show i => Show (Dd i) where
  show (Dd n) = unwords ["D", show n]

instance Show i => Show (Sd v i) where
  show (Sd _ _ sdim) = unwords ["S", show sdim]






  
  
