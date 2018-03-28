{-|
Module      : Data.Dim.Generic
Description : Dimension data, parametrized by an arbitrary container type
Copyright   : (c) Marco Zocca, 2018
License     : GPL-3
Maintainer  : zocca.marco gmail
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module Data.Dim.Generic where

-- | To define a /dense/ dimension we only need the dimensionality parameter
newtype Dd i = Dd {
   -- | Dimensionality
    dDim :: i
  } deriving (Eq)

-- | To define a /sparse/ dimension we need a cumulative array, an index array and a dimensionality parameter
data Sd v i = Sd {
      -- | Cumulative array (# nonzero entries per degree of freedom). Not all storage formats (e.g. COO for rank-2 tensors) need this information.
      sCml :: Maybe (v i)
      -- | Index array (indices of nonzero entries)
    , sIdx :: v i
      -- | Dimensionality 
    , sDim :: i
    } deriving (Eq)

dim :: Integral i => Either (Dd i) (Sd v i) -> Int
dim = fromIntegral . either dDim sDim 

instance Show i => Show (Dd i) where
  show (Dd i) = unwords ["D", show i]

instance (Show (v i), Show i) => Show (Sd v i) where
  show (Sd _ _ sdim) = unwords ["S", show sdim]

