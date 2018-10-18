{-# language TypeOperators, TypeFamilies #-}
module Data.Shape.Types (
  Z, (:#), (:.), Shape(..), TShape(..), trank, I, Ix
  , COO(..), compareIxCOO, Nz(..), fromListNz
                        ) where

import Data.Int (Int32)
import qualified Data.List.NonEmpty as NE
import Data.Ord

-- import Control.Arrow 

data Z
data sh :# e -- dense
data sh :. e -- sparse


-- | A class for data that have a shape.
class Shape t where
  -- type ShapeT t :: *
  -- shape :: t -> ShapeT t
  rank :: t -> Int
  dim :: t -> [Int]


class TShape t where
  -- | Tensor dimensions: (covariant, contravariant)
  tdim :: t -> ([Int], [Int])

-- | Tensor rank: (covariant, contravariant)
trank :: TShape t => t -> (Int, Int)
trank tt = (length co , length contra) where
  (co, contra) = tdim tt



-- | Index labels (e.g. 'i', 'j' .. ) are here encoded with integers
type I = Int

-- | Here we fix the size of the address space.
--
-- @type Ix = Int32@
type Ix = Int32

-- | Row types that can be indexed via an integer parameter
class COO r where
  type COOEl r :: *
  ixCOO :: I -> r -> Ix
  mkCOO :: [Ix] -> COOEl r -> r
  cooElem :: r -> COOEl r

instance COO (Nz a) where
  type COOEl (Nz a) = a
  ixCOO = ixUnsafe
  mkCOO = fromListNz
  cooElem = nzEl

compareIxCOO :: COO r => I -> r -> r -> Ordering
compareIxCOO j = comparing (ixCOO j)  


-- | A nonzero element in coordinate form
data Nz a = Nz {
    nzIxs :: !(NE.NonEmpty Ix)
  , nzEl :: a } deriving (Eq, Show)

fromListNz :: [Ix] -> a -> Nz a
fromListNz = Nz . NE.fromList


-- | Unsafe : it assumes the index is between 0 and (length - 1)
ixUnsafe :: I -> Nz a -> Ix
ixUnsafe i (Nz ne _) = ne NE.!! i  

-- ixUnsafeTup3 :: Int -> (Ix, Ix, Ix) -> Ix
-- ixUnsafeTup3 i (a, b, c) = case i of
--   0 -> a
--   1 -> b
--   2 -> c
--   _ -> error "derp"

-- | Unsafe : it assumes the index is between 0 and (length - 1)
compareIx :: I -> Nz a -> Nz a -> Ordering
compareIx i = comparing (ixUnsafe i)
