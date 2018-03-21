-- {-# language GADTs, TypeOperators #-}
-- {-# language TypeFamilies #-}
module Data.Dim.Named where

import Data.Vector.Unboxed as V


-- | To define a /dense/ dimension we only need the dimensionality parameter
data Ddn n i = Ddn {
   -- | Dimensionality
    dnDim :: i
   -- | Index label
  , dnIx :: n } deriving (Eq, Show)

-- | To define a /sparse/ dimension we need a cumulative array, an index array and a dimensionality parameter
data Sdn n i = Sdn {
      -- | Cumulative array (# nonzero entries per degree of freedom). Not all storage formats (e.g. COO for rank-2 tensors) need this information.
      snCml :: Maybe (V.Vector i)
      -- | Index array (indices of nonzero entries)
    , snIdx :: V.Vector i
      -- | Dimensionality 
    , snDim :: i
      -- | Index label
    , snIx :: n
    } deriving (Eq, Show)

