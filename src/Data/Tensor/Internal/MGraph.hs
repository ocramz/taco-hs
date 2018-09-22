{-|
Module      : Data.Tensor.Internal.MGraph
Description : Multigraphs, for representing TACO iteration graphs
Copyright   : (c) Marco Zocca, 2018
License     : GPL-3
Maintainer  : ocramz fripost org
Stability   : experimental
Portability : POSIX

MGraph is a multigraph (i.e. a graph in which each pair of nodes could be connected by > 1 edge)
-}
-- {-# language GADTs #-}
module Data.Tensor.Internal.MGraph where

import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE


data G a =
  Node a
  | Cons a (G a) deriving (Eq, Show)

newtype MGraph l a = MGraph {
  unMGraph :: M.Map l (G a)
                         } deriving (Eq, Show)




-- | 

-- data G nl a = G {
--     gNodes :: M.Map nl a
--   , gArcs :: M.Map a (NE.NonEmpty nl)
--              } deriving (Eq, Show)

-- singletonNode :: nl -> a -> G nl a
-- singletonNode nl x = G (M.singleton nl x) M.empty

-- lookupNode :: Ord k => k -> G k a -> Maybe a
-- lookupNode nl g = M.lookup nl (gNodes g)

-- -- addArc (G gn ga) x y = undefined


-- | -- 


-- newtype MGraph l a = MGraph (M.Map l (NE.NonEmpty a)) deriving (Eq, Show)


-- {-
-- node :: a
-- arc :: (a, a)
-- -}


-- data Node f a = Node {
--     node :: a
--   , neighbors :: f (Node f a)
--                    } 

-- singleL :: a -> Node [] a
-- singleL x = Node x [] 

-- -- -- | arrow from x to y
-- -- connect :: Node a -> Node a -> Node a
-- connect (Node x nx) y = Node x (y : nx) 

-- -- -- path (Node x nx) = x : path (head nx)
-- -- -- path (Node x []) = [x]

-- -- path (Node x nx) = case
-- --   nx of (nx0:_) -> x : path nx0
-- --         [] -> [x]

-- -- n0 = single 'z'
-- -- n1 = single 'a'






