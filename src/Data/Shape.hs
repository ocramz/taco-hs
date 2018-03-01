{-# language GADTs, TypeOperators #-}

module Data.Shape where

import Data.Monoid
import Data.Int (Int64, Int32)
-- import GHC.TypeLits
-- import GHC.Natural

import qualified Data.Vector.Unboxed as VU

import qualified Data.Dim as Dim


data Z
data sh :# e -- dense
data sh :. e -- sparse

-- | A statically-typed tensor shape parameter that supports both sparse and dense dimensions
data Sh sh where 
  Z :: Sh Z
  -- ^ Constructor for a dense dimension  
  D :: Sh sh -> Dim.D Int32 -> Sh (sh :# Int32)
  -- ^ Constructor for a sparse dimension 
  S :: Sh sh -> Dim.S Int32 -> Sh (sh :. Int32) 

type Dense1 = Z :# Int32
type Dense2 = (Z :# Int32) :# Int32
type CSR = (Z :# Int32) :. Int32
type COO = (Z :. Int32) :. Int32


instance Show (Sh sh) where
  show Z = ""
  show (D sh (Dim.D m)) = unwords [show m, show sh]
  show (S sh (Dim.S _ ix n)) = showSparse ix n <> show sh where
    showSparse ix n = show (VU.length ix, n)

instance Eq (Sh sh) where
  Z == Z = True
  (sh `D` d) == (sh2 `D` d2) = d == d2 && (sh == sh2)
  (sh `S` s) == (sh2 `S` s2) = s == s2 && (sh == sh2)

-- | rank
rank :: Sh sh -> Int
rank Z = 0
rank (D sh _) = 1 + rank sh
rank (S sh _) = 1 + rank sh

-- | dimensions
dim :: Sh sh -> [Integer]
dim Z = []
dim (D sh (Dim.D m)) = toInteger m : dim sh
dim (S sh (Dim.S _ _ m)) = toInteger m : dim sh


 

  
-- | Shape of a dense rank-2 tensor (a matrix)
mkD2 :: Int32 -> Int32 -> Sh ((Z :# Int32) :# Int32)
mkD2 m n = (Z `D` Dim.D m) `D` Dim.D n

-- | Shape of a rank-2 CSR tensor (dense in the first index, sparse in the second)
mkCSR :: Int32 -> Int32 -> VU.Vector Int32 -> VU.Vector Int32 -> Sh CSR
mkCSR m n icml iidx = (Z `D` Dim.D m) `S` Dim.S (Just icml) iidx n

-- | Shape of a rank-2 COO tensor (sparse in both indices)
mkCOO :: Int32 -> Int32 -> VU.Vector Int32 -> VU.Vector Int32 -> Sh COO
mkCOO m n vi vj = (Z `S` Dim.S Nothing vi m) `S` Dim.S Nothing vj n





-- | Playground

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
