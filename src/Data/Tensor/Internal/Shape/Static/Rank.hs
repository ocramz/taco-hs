{-# language GADTs, TypeOperators #-}
{-# language TypeFamilies #-}
module Data.Tensor.Internal.Shape.Static.Rank where

import Data.Monoid
import Data.Int (Int32)
-- import GHC.TypeLits
-- import GHC.Natural

import qualified Data.Vector.Unboxed as VU
import           GHC.TypeLits (Nat)

import qualified Data.Tensor.Internal.Dim as Dim
-- import Data.Shape.Types   


data Z
data sh :# e -- dense
data sh :. e -- sparse

-- | A statically-typed tensor shape parameter that supports both sparse and dense dimensions.
-- Dimensions are indexed with 'Int32' indices, which should be enough for most applications.
--
-- Note: in this formulation, only the /rank/ (i.e. the number of dimensions) is known at compile time. The actual dimensionality is only known at runtime.
-- For a formulation that addresses this, see Data.Shape.Static
data Sh v sh where 
  Z :: Sh v Z
  -- | Constructor for a dense dimension  
  D :: Sh v sh -> Dim.Dd Int32 -> Sh v (sh :# Int32)
  -- | Constructor for a sparse dimension 
  S :: Sh v sh -> Dim.Sd v Int32 -> Sh v (sh :. Int32)

type D1 = Z :# Int32
type S1 = Z :. Int32
type D2 = (Z :# Int32) :# Int32
type CSR = (Z :# Int32) :. Int32
type COO = (Z :. Int32) :. Int32


-- instance Show (Sh v sh) where
--   show Z = ""
--   show (D sh (Dim.Dd m)) = unwords [show m, show sh]
--   show (S sh (Dim.Sd _ ix n)) = showSparse ix n <> show sh where
--     showSparse ixx nn = show (VU.length ixx, nn)

-- instance Eq (v Int32) => Eq (Sh v sh) where
--   Z == Z = True
--   (sh `D` d) == (sh2 `D` d2) = d == d2 && (sh == sh2)
--   (sh `S` s) == (sh2 `S` s2) = s == s2 && (sh == sh2)

-- | Rank of a shape (i.e. number of dimensions)
rank_ :: Sh v sh -> Int
rank_ Z = 0
rank_ (D sh _) = 1 + rank_ sh
rank_ (S sh _) = 1 + rank_ sh

-- | Dimension of a shape (i.e. list of dimension sizes)
dim_ :: Sh v sh -> [Int]
dim_ Z = []
dim_ (D sh (Dim.Dd m)) = fromIntegral m : dim_ sh
dim_ (S sh (Dim.Sd _ _ m)) = fromIntegral m : dim_ sh





-- -- | Shape of a dense vector
-- mkD1 :: Int32 -> Sh D1
-- mkD1 m = Z `D` Dim.Dd m

-- -- | Shape of a sparse vector
-- mkS1 :: Int32 -> VU.Vector Int32 -> VU.Vector Int32 -> Sh S1
-- mkS1 m segv ixv = Z `S` Dim.Sd (Just segv) ixv m
  
-- -- | Shape of a dense rank-2 tensor (a matrix)
-- mkD2 :: Int32 -> Int32 -> Sh D2
-- mkD2 m n = (Z `D` Dim.Dd m) `D` Dim.Dd n

-- -- | Shape of a rank-2 CSR tensor (dense in the first index, sparse in the second)
-- mkCSR :: Int32 -> Int32 -> VU.Vector Int32 -> VU.Vector Int32 -> Sh CSR
-- mkCSR m n icml iidx = (Z `D` Dim.Dd m) `S` Dim.Sd (Just icml) iidx n

-- -- | Shape of a rank-2 COO tensor (sparse in both indices)
-- mkCOO :: Int32 -> Int32 -> VU.Vector Int32 -> VU.Vector Int32 -> Sh COO
-- mkCOO m n vi vj = (Z `S` Dim.Sd Nothing vi m) `S` Dim.Sd Nothing vj n










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
