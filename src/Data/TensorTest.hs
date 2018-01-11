module Data.TensorTest where

import qualified Data.Vector.Unboxed as V
import Data.Tensor






-- reduce imode mata matb 





-- | sparse vector /addition/ as 2-way merge
spAdd :: (Num a, Ord i) => [(i, a)] -> [(i, a)] -> [(i, a)]
spAdd = go where
  go [] y = y
  go x [] = x
  go xv@((i,x):xs) yv@((j,y):ys) = 
    case compare i j of EQ -> (i, x + y) : go xs ys
                        LT -> (i, x)     : go xs yv
                        GT -> (j, y)     : go xv ys

-- | sparse vector /component-wise multiplication/ as 2-way merge
spMul :: (Num a, Ord i) => [(i, a)] -> [(i, a)] -> [(i, a)]
spMul = go where
  go [] _  = []
  go _  [] = []
  go xv@((i,x):xs) yv@((j,y):ys) =
    case compare i j of EQ -> (i, x * y) : go xs ys
                        LT -> go xs yv
                        GT -> go xv ys
                        

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


