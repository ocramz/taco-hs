{-# language LambdaCase, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-|
Module      : Data.Dim
Description : Tensor dimension metadata
Copyright   : (c) Marco Zocca, 2018
License     : GPL-3
Maintainer  : ocramz fripost org
Stability   : experimental
Portability : POSIX

This module contains types and construction/access functions for tensor dimension metadata which are declared to be polymorphic in the container type (i.e. could be lists, vectors etc.).
Note : no rank or dimensionality information is known at compile time, that is, size mismatch errors will have to be raised at runtime.
-}
module Data.Dim (
  -- * Dimension metadata  
    DimE(..), dimE, denseDimE, sparseDimE
  , Dd(..), Sd(..)
  ) where

-- import Control.Applicative hiding (empty)
-- import Data.Maybe (isJust)
-- import Control.Arrow ((***))
-- import Data.Int (Int32(..), Int64(..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.IntMap as IM
import qualified Data.Map.Strict as M

{- 
Tensor product shorthand (Einstein notation) prescribes that only pairs of tensors with common indices can be multiplied. In particular, in the index pair one index should be variant and the other contravariant.
-}

-- | Tensor dimensions can be either dense or sparse
newtype DimE v i = DimE {
  unDimE :: Either (Dd i) (Sd v i)
  } deriving (Eq, Show)

dimE :: DimE v i -> i
dimE (DimE ei) = either dDim sDim ei

-- instance Show i => Show (DimE v i) where
--   show (DimE ei) = either shd shs ei where
--     shd (Dd n) = unwords ["D", show n]
--     shs (Sd _ _ n) = unwords ["S", show n]

-- | Construct a dense DimE
denseDimE :: i -> DimE v i
denseDimE = DimE . Left . Dd 

-- | Construct a sparse DimE
sparseDimE :: v i -> v i -> i -> DimE v i
sparseDimE sv ixv n = DimE (Right (Sd sv ixv n))


-- | To define a /dense/ dimension we only need the dimensionality parameter (an integer)
newtype Dd i = Dd {
   -- | Dimensionality
    dDim :: i
  } deriving (Eq, Show)

-- | To define a /sparse/ dimension we need a cumulative array, an index array and a dimensionality parameter
data Sd v i = Sd {
      -- | Location in the sIdx array where each segment begins. 
      sPtr :: v i
      -- | Index array (indices of nonzero entries)
    , sIdx :: v i
      -- | Dimensionality 
    , sDim :: i
    } deriving (Eq, Show)

-- instance Show i => Show (Dd i) where
--   show (Dd n) = unwords ["D", show n]

-- instance Show i => Show (Sd v i) where
--   show (Sd _ _ sdim) = unwords ["S", show sdim]






  
  
