module Data.Dim.Named.Generic where

-- | To define a /dense/ dimension we only need the dimensionality parameter
data Ddn n i = Ddn {
   -- | Dimensionality
    dnDim :: i
   -- | Index label
  , dnIx :: n } deriving (Eq, Show)

-- | To define a /sparse/ dimension we need a cumulative array, an index array and a dimensionality parameter
data Sdn n v i = Sdn {
      -- | Cumulative array (# nonzero entries per degree of freedom). Not all storage formats (e.g. COO for rank-2 tensors) need this information.
      snCml :: Maybe (v i)
      -- | Index array (indices of nonzero entries)
    , snIdx :: v i
      -- | Dimensionality 
    , snDim :: i
      -- | Index label
    , snIx :: n
    } deriving (Eq, Show)

dim :: Either (Ddn n c) (Sdn n v c) -> c
dim = either dnDim snDim
