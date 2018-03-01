{-# language MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
{-# language TypeOperators #-}
{-# language GADTs #-}
module Data.TensorTest where

import Data.List (splitAt, unfoldr)

import qualified Data.Vector.Unboxed as V
-- import Data.Tensor


data Exp a where
  Lift :: (a -> b) -> Exp a -> Exp b


-- | tensor elements may be indexed
class Ord (Ix a) => Elem a where
  type Ix a :: *
  type Ev a :: *
  proj :: a -> (Ix a, Ev a)
  prod :: Ix a -> Ev a -> a


data E1 a = E1 Int a deriving Show

instance Elem (E1 a) where
  type Ix (E1 a) = Int
  type Ev (E1 a) = a
  proj (E1 i x) = (i, x)
  prod i x = E1 i x

data E2 a = E2 Int Int a deriving Show

instance Elem (E2 a) where
  type Ix (E2 a) = (Int, Int)
  type Ev (E2 a) = a
  proj (E2 i j x) = ((i, j), x)
  prod (i, j) x = E2 i j x




      




spUnion' :: Elem a => (Ev a -> Ev a -> Ev a) -> [a] -> [a] -> [a]
spUnion' ff = go where
  go [] y = y
  go x [] = x
  go xv@(elx:xs) yv@(ely:ys) =
    let
      (ix, elvx) = proj elx
      (iy, elvy) = proj ely
    in 
      case compare ix iy of
        EQ -> (prod ix (ff elvx elvy)) : go xs ys
        LT -> prod ix elvx             : go xs yv
        GT -> prod iy elvy             : go xv ys
        




-- | sparse vector /union/ as 2-way merge
spUnion :: Ord i => (a -> a -> a) -> [(i, a)] -> [(i, a)] -> [(i, a)]
spUnion ff = go where
  go [] y = y
  go x [] = x
  go xv@((i,x):xs) yv@((j,y):ys) = 
    case compare i j of EQ -> (i, ff x y) : go xs ys
                        LT -> (i, x)      : go xs yv
                        GT -> (j, y)      : go xv ys

-- | sparse vector /component-wise intersection/ as 2-way merge
spIntersect :: Ord i => (a -> a -> a) -> [(i, a)] -> [(i, a)] -> [(i, a)]
spIntersect gg = go where
  go [] _  = []
  go _  [] = []
  go xv@((i,x):xs) yv@((j,y):ys) =
    case compare i j of EQ -> (i, gg x y) : go xs ys
                        LT -> go xs yv
                        GT -> go xv ys

spAdd :: (Num a, Ord i) => [(i, a)] -> [(i, a)] -> [(i, a)]  
spAdd = spUnion (+)

spMul :: (Num a, Ord i) => [(i, a)] -> [(i, a)] -> [(i, a)]
spMul = spIntersect (*)


v0, v1 :: [(Int, Int)]
v0 = [(0, 1), (2, 2), (5, 1)]
v1 = [(0, 2), (1, 3), (2, 3), (4, 1)]



-- | "AND"
conjunction :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
conjunction f a b = case (a, b) of
  (Just x, Just y)  -> Just $ f x y
  _                 -> Nothing

-- | "OR"
disjunction :: (t -> t -> t) -> Maybe t -> Maybe t -> Maybe t
disjunction f a b = case (a, b) of
  (Nothing, Nothing) -> Nothing
  (Just x,  Nothing) -> Just x
  (Nothing, Just y)  -> Just y
  (Just x, Just y)   -> Just $ f x y

sumMaybe :: Num a => Maybe a -> Maybe a -> Maybe a  
sumMaybe = disjunction (+)

prodMaybe :: Num a => Maybe a -> Maybe a -> Maybe a  
prodMaybe = conjunction (*)





-- chunk sparse lists according to element index

-- chunkBy q ll = go ll []
--   where
--     go _ acc = acc
--     go (e:es) acc
--       | q e       = go es (e : acc)
--       -- | otherwise =

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n ll = h : chunks n t where
  (h, t) = splitAt n ll

chunksWhile :: (a -> Bool) -> [a] -> [[a]]
chunksWhile _ [] = []
chunksWhile q ll = h : chunksWhile q t where
  (h, t) = (takeWhile q ll, dropWhile q ll)

chunksWhile' :: (a -> Bool) -> [a] -> [[a]]
chunksWhile' q = unfoldr genf where
  genf ll =
    if null h
      then Nothing
      else Just (h, drop (length h) ll)
    where h = takeWhile q ll 
