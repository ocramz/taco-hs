{-|
Module      : Data.Dim
Description : Dimension data, stored as a Vector.Unboxed
Copyright   : (c) Marco Zocca, 2018
License     : GPL-3
Maintainer  : ocramz fripost org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module Data.Dim where

import Data.Vector.Unboxed as V
-- import qualified Data.Dim.Generic as DG

-- * Dimension metadata

-- data family Dim a
-- data instance Dim (Dd i)
-- data instance Dim (Sd i)

-- | To define a /dense/ dimension we only need the dimensionality parameter
newtype Dd i = Dd { dDim :: i } deriving (Eq, Show)

-- | To define a /sparse/ dimension we need a cumulative array, an index array and a dimensionality parameter
data Sd i = Sd {
      -- | Cumulative array (# nonzero entries per degree of freedom). Not all storage formats (e.g. COO for rank-2 tensors) need this information.
      sCml :: Maybe (V.Vector i)
      -- | Index array (indices of nonzero entries)
    , sIdx :: V.Vector i
      -- | Size of the tensor along this dimension
    , sDim :: i } deriving (Eq, Show)

 
-- | A tensor dimension can be either dense or sparse.
--
-- Example: the CSR format is /dense/ in the first index (rows) and /sparse/ in the second index (columns)

dim :: Either (Dd c) (Sd c) -> c
dim = either dDim sDim


  
  
