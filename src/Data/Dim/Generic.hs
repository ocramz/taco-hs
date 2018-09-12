{-|
Module      : Data.Dim.Generic
Description : Dimension data, parametrized by an arbitrary container type
Copyright   : (c) Marco Zocca, 2018
License     : GPL-3
Maintainer  : ocramz fripost org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module Data.Dim.Generic where

import Data.Shape.Types


newtype DimsE v e = DimsE [DimE v e] deriving (Eq, Show)

instance Shape (DimsE v e) where
  dim (DimsE de) = map dimE de
  rank = product . dim

  
-- | Tensor dimensions can be either dense or sparse
newtype DimE v e = DimE {
  unDimE :: Either Dd (Sd v e)
  } deriving (Eq)

dimE :: DimE v e -> Int
dimE (DimE ei) = either dDim sDim ei

instance Show (DimE v e) where
  show (DimE ei) = either shd shs ei where
    shd (Dd n) = unwords ["dense :", show n]
    shs (Sd _ _ n) = unwords ["sparse :", show n]

-- | Construct a dense DimE
denseDimE :: Int  -> DimE v e
denseDimE = DimE . Left . Dd 

-- | Construct a sparse DimE
sparseDimE :: Maybe (v e) -> v e -> Int -> DimE v e
sparseDimE sv ixv n = DimE (Right (Sd sv ixv n))


-- | To define a /dense/ dimension we only need the dimensionality parameter (an integer)
newtype Dd = Dd {
   -- | Dimensionality
    dDim :: Int
  } deriving (Eq)

-- | To define a /sparse/ dimension we need a cumulative array, an index array and a dimensionality parameter
data Sd v e = Sd {
      -- | Cumulative array (# nonzero entries per degree of freedom). Not all storage formats (e.g. COO for rank-2 tensors) need this information. One can also view this array as storing the location in the Idx array where each segment begins.
      sCml :: Maybe (v e)
      -- | Index array (indices of nonzero entries)
    , sIdx :: v e
      -- | Dimensionality 
    , sDim :: Int
    } deriving (Eq)

-- dim :: Integral i => Either (Dd i) (Sd v i) -> Int
-- dim = fromIntegral . either dDim sDim 

instance Show Dd where
  show (Dd n) = unwords ["D", show n]

instance (Show (v i), Show i) => Show (Sd v i) where
  show (Sd _ _ sdim) = unwords ["S", show sdim]

