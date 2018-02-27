{-# language GADTs, TypeOperators #-}
module Data.Dim where

import Data.Vector.Unboxed as V


-- * Dimension metadata

-- | To define a /dense/ dimension we only need the dimensionality parameter
newtype D i = D { dDim :: Int } deriving (Eq, Show)


-- | To define a /sparse/ dimension we need a cumulative array, an index array and a dimensionality parameter
data S i = S {
      sCml :: Maybe (V.Vector i)
    , sIdx :: V.Vector i
    , sDim :: Int }
  deriving (Eq, Show)

-- | A tensor dimension can be either dense or sparse.
--
-- Example: the CSR format is /dense/ in the first index (rows) and /sparse/ in the second index (columns)

-- newtype DMD i = DMD (Either (DMDDense i) (DMDSparse i)) deriving (Eq, Show)


  
  
