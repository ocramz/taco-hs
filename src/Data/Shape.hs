{-# language GADTs, TypeOperators #-}

module Data.Shape where

import Data.Monoid 
-- import GHC.TypeLits
-- import GHC.Natural

import qualified Data.Vector.Unboxed as VU

import qualified Data.Dim as Dim



data Z
data sh :# e -- dense
data sh :. e -- sparse

data Sh sh where
  Z :: Sh Z
  -- ^ Constructor for a dense dimension  
  D :: Sh sh -> Dim.D Int -> Sh (sh :# Int)
  -- ^ Constructor for a sparse dimension
  S :: Sh sh -> Dim.S Int -> Sh (sh :. Int)

rank :: Sh sh -> Int
rank Z = 0
rank (D sh _) = 1 + rank sh
rank (S sh _) = 1 + rank sh

dim :: Sh sh -> [Int]
dim Z = []
dim (D sh (Dim.D m)) = m : dim sh
dim (S sh (Dim.S _ _ m)) = m : dim sh


instance Show (Sh sh) where
  show Z = ""
  show (D sh (Dim.D m)) = unwords [show m, show sh]
  show (S sh (Dim.S _ ix n)) = showSparse ix n <> show sh where
    showSparse ix n = show (VU.length ix, n)

instance Eq (Sh sh) where
  Z == Z = True
  (sh `D` d) == (sh2 `D` d2) = d == d2 && (sh == sh2)
  (sh `S` s) == (sh2 `S` s2) = s == s2 && (sh == sh2)


-- newtype D2 i = D2 (Shape ((Z :. i) :. i))
  
-- | Shape of a dense rank-2 tensor (a matrix)
-- mkD2 :: Int -> Int -> Sh ((Z :# i1) :# i)
mkD2 m n = (Z `D` Dim.D m) `D` Dim.D n

-- | Shape of a rank-2 CSR matrix (dense in the first index, sparse in the second)
-- mkCSR :: Int -> VU.Vector i -> VU.Vector i -> Int -> Sh ((Z :# i) :. i)
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
