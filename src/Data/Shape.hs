{-# language GADTs, TypeOperators #-}

module Data.Shape where

import Data.Monoid
import Data.Int (Int32)
-- import GHC.TypeLits
-- import GHC.Natural

import qualified Data.Vector.Unboxed as VU

import qualified Data.Dim as Dim


data Z
data sh :# e -- dense
data sh :. e -- sparse

-- | A statically-typed tensor shape parameter that supports both sparse and dense dimensions.
-- Dimensions are indexed with 'Int32' indices, which should be enough for most applications.
data Sh sh where 
  Z :: Sh Z
  -- | Constructor for a dense dimension  
  D :: Sh sh -> Dim.Dd Int32 -> Sh (sh :# Int32)
  -- | Constructor for a sparse dimension 
  S :: Sh sh -> Dim.Sd Int32 -> Sh (sh :. Int32) 

type D1 = Z :# Int32
type D2 = (Z :# Int32) :# Int32
type CSR = (Z :# Int32) :. Int32
type COO = (Z :. Int32) :. Int32


instance Show (Sh sh) where
  show Z = ""
  show (D sh (Dim.Dd m)) = unwords [show m, show sh]
  show (S sh (Dim.Sd _ ix n)) = showSparse ix n <> show sh where
    showSparse ixx nn = show (VU.length ixx, nn)

instance Eq (Sh sh) where
  Z == Z = True
  (sh `D` d) == (sh2 `D` d2) = d == d2 && (sh == sh2)
  (sh `S` s) == (sh2 `S` s2) = s == s2 && (sh == sh2)

-- | Rank of a shape (i.e. number of dimensions)
rank :: Sh sh -> Int
rank Z = 0
rank (D sh _) = 1 + rank sh
rank (S sh _) = 1 + rank sh

-- | Dimension of a shape (i.e. list of dimension sizes)
dim :: Sh sh -> [Integer]
dim Z = []
dim (D sh (Dim.Dd m)) = toInteger m : dim sh
dim (S sh (Dim.Sd _ _ m)) = toInteger m : dim sh




  
-- | Shape of a dense rank-2 tensor (a matrix)
mkD2 :: Int32 -> Int32 -> Sh D2
mkD2 m n = (Z `D` Dim.Dd m) `D` Dim.Dd n

-- | Shape of a rank-2 CSR tensor (dense in the first index, sparse in the second)
mkCSR :: Int32 -> Int32 -> VU.Vector Int32 -> VU.Vector Int32 -> Sh CSR
mkCSR m n icml iidx = (Z `D` Dim.Dd m) `S` Dim.Sd (Just icml) iidx n

-- | Shape of a rank-2 COO tensor (sparse in both indices)
mkCOO :: Int32 -> Int32 -> VU.Vector Int32 -> VU.Vector Int32 -> Sh COO
mkCOO m n vi vj = (Z `S` Dim.Sd Nothing vi m) `S` Dim.Sd Nothing vj n





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
