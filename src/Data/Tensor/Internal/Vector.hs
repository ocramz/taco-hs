module Data.Tensor.Internal.Vector where

import Data.Int (Int32, Int64)
import Data.Foldable (foldl')
import Data.List (group, groupBy)
import qualified Data.Vector.Algorithms.Radix as VSR (sort, sortBy, Radix(..))
import qualified Data.Vector.Algorithms.Merge as VSM (sort, sortBy, Comparison)
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Mutable as VM
-- import qualified Data.IntMap as IM
import Control.Monad.Primitive
import Control.Monad.ST

import Data.Function (on)
import Data.Ord
import qualified Data.List.NonEmpty as NE
-- import Prelude hiding ( (!!), length )


newtype Row a = Row { unRow :: NE.NonEmpty a } deriving (Eq, Show)

ix :: Int -> Row a -> Maybe a
ix i (Row ne)
  | i > NE.length ne || i < 0 = Nothing
  | otherwise = Just $ ne NE.!! i

-- | NB : assumes the index is between 0 and (length - 1)
ixUnsafe :: Int -> Row a -> a
ixUnsafe i (Row ne) = ne NE.!! i  

-- | NB : assumes the index is between 0 and (length - 1)
compareIx :: Ord a => Int -> Row a -> Row a -> Ordering
compareIx i = comparing (ixUnsafe i)







ptrV ::
     Int   -- ^ Number of rows
  -> V.Vector Int   -- ^ NNZ in each row
  -> V.Vector Int  -- ^ Pointer vector
ptrV = csPtrV (==)

-- | Given a number of rows(resp. columns) `n` and a _sorted_ Vector of Integers in increasing order (containing the column (resp. row) indices of nonzero entries), return the cumulative vector of nonzero entries of length `n + 1` (the "column (resp. row) pointer" of the CSR(CSC) format). NB: Fused count-and-accumulate
-- E.g.:
-- > csPtrV (==) 4 (V.fromList [1,1,2,3])
-- [0,0,2,3,4]
csPtrV :: (a -> Int -> Bool) -> Int -> V.Vector a -> V.Vector Int
csPtrV eqf n xs = V.create createf where
  createf :: ST s (VM.MVector s Int)
  createf = do
    let c = 0
    vm <- VM.new (n + 1)
    VM.write vm 0 0  -- write `0` at position 0
    let loop v ll i count | i == n = return ()
                          | otherwise = do
                              let lp = V.length $ V.takeWhile (`eqf` i) ll
                                  count' = count + lp
                              VM.write v (i + 1) count'
                              loop v (V.drop lp ll) (succ i) count'
    loop vm xs 0 c
    return vm
