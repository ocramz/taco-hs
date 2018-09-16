{-|
MGraph is a multigraph (i.e. a graph in which each pair of nodes could be connected by > 1 edge)
-}
module Data.Tensor.Internal.MGraph where

import qualified Data.Map.Strict as M




newtype MGraph l a = MGraph (M.Map l a) deriving (Eq, Show)
