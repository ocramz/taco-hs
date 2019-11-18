{-# language TypeOperators, TypeFamilies #-}
module Data.Tensor.Internal.Shape.Types (
  -- Z, (:#), (:.),
  Shape(..), TShape(..), trank, I, Ix
  , COO(..), compareIxCOO, Nz, nz
                        ) where

import Data.Int (Int32)
import qualified Data.List.NonEmpty as NE
import Data.Ord

-- import Control.Arrow 


-- | Properties of operands that have a shape.
class Shape t where
  -- type ShapeT t :: *
  -- shape :: t -> ShapeT t
  rank :: t -> Int
  dim :: t -> [Int]

-- | Properties of tensors 
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

-- | Tensor elements in COOrdinate encoding 
class COO r where
  type COOEl r :: *
  -- | Given an index and a tensor element, return the corresponding coordinate
  ixCOO :: I -> r -> Ix
  -- | Construct a tensor element from a list of coordinates and a value
  mkCOO :: [Ix] -> COOEl r -> r
  -- | Return the value stored in the given tensor element
  cooElem :: r -> COOEl r

instance COO (Nz a) where
  type COOEl (Nz a) = a
  ixCOO = ixUnsafe
  mkCOO = nz
  cooElem = nzEl

-- | Compare two rows by ordering on the I'th index
compareIxCOO :: COO r => I -> r -> r -> Ordering
compareIxCOO j = comparing (ixCOO j)  


-- | A nonzero element of a sparse tensor in coordinate form
data Nz a = Nz {
    nzIxs :: !(NE.NonEmpty Ix)  -- ^ Element coordinates
  , nzEl :: a   -- ^ Value
  } deriving (Eq, Show)

-- | Construct a 'Nz' element from a list of coordinates and a value
nz :: [Ix] -> a -> Nz a
nz = Nz . NE.fromList


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
