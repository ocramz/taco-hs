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

import Data.List (unfoldr)

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.List.NonEmpty as NE



data Op a = Reduce a a | Kron a a deriving (Eq, Show)

inner uu vv = sum $ zipWith (*) uu vv

inner' :: (Foldable f, Num a) => f (a, a) -> a
inner' = foldr ins 0 where
  ins (u, v) z = z + (u * v)

-- | Kronecker product
--
-- >>> kron [2,3,1,2] [2,3,4,5]
-- [[4,6,8,10],[6,9,12,15],[2,3,4,5],[4,6,8,10]]
-- kron = unfoldr go [] where
--   go ((uu, vv) : _)
--     | null uu = Nothing
    
-- kron uvs = unfoldr go uvs where
--   go ((uu, vv):_) | null uu = Nothing
--                   | otherwise = Just ((head uu *) `map` vv, (tail uu, vv))
-- kron :: (Functor f, Functor g, Num a) => f a -> g a -> f (g a)
-- kron uu vv = fmap (\u -> (u *) <$> vv) uu


-- | 

-- newtype G a = G { unG :: IM.IntMap [a] } deriving (Eq, Show)

-- unionConcat :: G a -> G a -> G a
-- unionConcat g1 g2 = G $ IM.unionWith (++) (unG g1) (unG g2)

-- fromList :: [a] -> G a
-- fromList xs = G $ IM.fromList $ zip [0 .. ] xs' where xs' = map pure xs

-- size :: G a -> Int
-- size (G im) = IM.size im  

-- mapKeys :: (IM.Key -> IM.Key) -> G a -> G a
-- mapKeys f (G im) = G $ IM.mapKeys f im

-- pairAt i g1 g2 = undefined where
--   (sz1, sz2) = (size g1, size g2)
--   g2' = mapKeys (+ i) g2
  






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






