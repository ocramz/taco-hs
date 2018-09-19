{-|
MGraph is a multigraph (i.e. a graph in which each pair of nodes could be connected by > 1 edge)
-}
module Data.Tensor.Internal.MGraph where

import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE


newtype MGraph l a = MGraph (M.Map l (NE.NonEmpty a)) deriving (Eq, Show)



{-
node :: a
arc :: (a, a)
-}


data Node f a = Node {
    node :: a
  , neighbors :: f (Node f a)
                   } 

singleL :: a -> Node [] a
singleL x = Node x [] 

-- -- | arrow from x to y
-- connect :: Node a -> Node a -> Node a
connect (Node x nx) y = Node x (y : nx) 

-- -- path (Node x nx) = x : path (head nx)
-- -- path (Node x []) = [x]

-- path (Node x nx) = case
--   nx of (nx0:_) -> x : path nx0
--         [] -> [x]

-- n0 = single 'z'
-- n1 = single 'a'
