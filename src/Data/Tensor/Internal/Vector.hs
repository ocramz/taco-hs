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

-- | Unsafe : it assumes the index is between 0 and (length - 1)
ixUnsafe :: Int -> Row a -> a
ixUnsafe i (Row ne) = ne NE.!! i  

-- | Unsafe : it assumes the index is between 0 and (length - 1)
compareIx :: Ord a => Int -> Row a -> Row a -> Ordering
compareIx i = comparing (ixUnsafe i)





-- | Given a number of rows(resp. columns) `n` and a _sorted_ Vector of Integers in increasing order (containing the column (resp. row) indices of nonzero entries), return the cumulative vector of nonzero entries of length `n` (the "column (resp. row) pointer" of the CSR(CSC) format). NB: Fused count-and-accumulate
-- E.g.:
-- > csPtrV 4 (V.fromList [1,1,2,3])
-- [0,2,3,4]
csPtrV :: Int32 -> V.Vector Int32 -> V.Vector Int32
csPtrV n xs = V.create createf where
  createf :: ST s (VM.MVector s Int32)
  createf = do
    let c = 0
    vm <- VM.new (fromIntegral n)
    VM.write vm 0 0  -- write `0` at position 0
    let loop v ll i count | i == n = return ()
                          | otherwise = do
                              let lp = V.length $ V.takeWhile (== i) ll
                                  count' = count + lp
                              VM.write v (fromIntegral i) (fromIntegral count')
                              loop v (V.drop lp ll) (succ i) count'
    loop vm xs 1 c
    return vm



-- asdf :: PrimMonad m =>
--         Int32
--      -> VM.MVector (PrimState m) Int32
--      -> V.Vector Int32
--      -> m ()
-- asdf n vm xs = loop vm xs 1 0 where
--   loop v ll i count | i == n = return ()
--                     | otherwise = do
--                         let lp = V.length $ V.takeWhile (== i) ll
--                             count' = count + lp
--                         VM.write v (fromIntegral i) (fromIntegral count')
--                         loop v (V.drop lp ll) (succ i) count'
