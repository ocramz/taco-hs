{-# language GADTs, TypeOperators #-}

module Data.Shape where

-- import GHC.TypeLits
-- import GHC.Natural

import qualified Data.Vector.Unboxed as VU

import qualified Data.Dim as Dim



data Z
data sh :. e

data Shape sh where
  Z :: Shape Z
  S :: Shape sh -> Dim.S i -> Shape (sh :. i)
  D :: Shape sh -> Dim.D i -> Shape (sh :. i) 

newtype D2 i = D2 (Shape ((Z :. i) :. i)) 

mkD2 :: Int -> Int -> Shape ((Z :. i1) :. i)
mkD2 m n = (Z `D` Dim.D m) `D` Dim.D n

mkCSR :: Int -> VU.Vector i -> VU.Vector i -> Int -> Shape ((Z :. i) :. i)
mkCSR m icml iidx n = (Z `D` Dim.D m) `S` Dim.S (Just icml) iidx n


-- data Z
-- data Sparse sh e
-- data Dense sh e

-- data Shape sh where
--   Z :: Shape Z
--   S :: Shape sh -> Dim.D i -> Shape (Sparse sh i)
--   D :: Shape sh -> Dim.S i -> Shape (Dense sh i)


-- data Z
-- data sh :. e

-- data Shape sh where
--   Z :: Shape Z
--   (:.) :: Shape sh -> i -> Shape (sh :. i)

-- dimz :: Shape sh -> Int
-- dimz Z = 0
-- dimz (sh :. _) = dimz sh + 1




-- -- | An index of dimension zero
-- data Z  = Z
--         deriving (Show, Read, Eq, Ord)

-- -- | Our index type, used for both shapes and indices.
-- infixl 3 :.
-- data tail :. head = !tail :. !head
--         deriving (Show, Read, Eq, Ord)



-- class Eq sh => Shape sh where
--   rank :: sh -> Int
--   -- listOfShape :: sh -> [Int]
--   -- shapeOfList :: [Int] -> sh

-- instance (Eq i, Shape sh) => Shape (sh :. i) where
--   rank (sh :. _) = rank sh + 1

-- instance Shape Z where
--   rank _ = 0
--   -- listOfShape _ = []
--   -- shapeOfList [] = Z
--   -- shapeOfList _ = error $ stage ++ ".fromList: non-empty list when converting to Z."
