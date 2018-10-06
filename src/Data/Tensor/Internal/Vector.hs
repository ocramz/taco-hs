{-# language TypeFamilies, FlexibleContexts #-}
module Data.Tensor.Internal.Vector where

import Data.Int (Int32)
-- import Data.Foldable (foldl')
-- import Data.List (group, groupBy)
-- import qualified Data.Vector.Algorithms.Radix as VSR (sort, sortBy, Radix(..))
import qualified Data.Vector.Algorithms.Merge as VSM (sortBy)
import qualified Data.Vector as V
-- import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Mutable as VM
-- import qualified Data.IntMap as IM
import Control.Monad.Primitive
import Control.Monad.ST

-- import Data.Function (on)
import Data.Ord
import qualified Data.List.NonEmpty as NE
-- import Prelude hiding ( (!!), length )

import Control.Parallel.Strategies (using, rpar, parTraversable)

import Data.Dim




-- | Row types that can be indexed via an integer parameter
class Row r where
  type RIxTy r :: *
  ixRow :: Int -> r -> RIxTy r

instance Row (Nz i a) where
  type RIxTy (Nz i a) = i
  ixRow = ixUnsafe 


-- | A nonzero element in coordinate form
data Nz i a = Nz {
    nzIxs :: !(NE.NonEmpty i)
  , nzEl :: a } deriving (Eq, Show)

fromList :: i -> [i] -> a -> Nz i a
fromList i iis = Nz (i NE.:| iis)


-- | Unsafe : it assumes the index is between 0 and (length - 1)
ixUnsafe :: Int -> Nz i a -> i
ixUnsafe i (Nz ne _) = ne NE.!! i  

-- | Unsafe : it assumes the index is between 0 and (length - 1)
compareIx :: Ord i => Int -> Nz i a -> Nz i a -> Ordering
compareIx i = comparing (ixUnsafe i)

-- compareIx :: (Row r, Ord (RIxTy r)) => Int -> r -> r -> Ordering
-- compareIx i = comparing (ixRow i)

-- | A @Vector (Nz i a)@ contains the coordinate representation of the nonzero entries in a tensor.
--
-- The compressed-sparse-fiber (CSF) pointer vectors are computed by sorting this representation over one of its indices and counting repeated indices (with @ptrV@).
--
-- For example, the CSF computation for a rank-3 sparse tensor will entail 3 sorts and 3 corresponding calls of @ptrV@.
--
-- In this implementation, we use parallel strategies to evaluate in parallel the sort-and-count.
csf :: (PrimMonad m, Traversable t) =>
                V.Vector (Nz Int32 a)
             -> t (Int, Int32)  -- ^ (Index, dimensionality)
             -> m (t (Sd V.Vector Int32))
csf v ixs = do
  vs <- traverse sortf ixs
  pure (vs `using` parTraversable rpar)
    where
      sortf (i, n) = do
        v' <- sortOnIx v i
        let vp = ptrV i n v'
            vi = ixUnsafe i <$> v
        pure $ Sd (Just vp) vi n

-- sortOnIx :: (PrimMonad m, Row r, Ord (RIxTy r)) => V.Vector r -> Int -> m (V.Vector r)
sortOnIx :: (PrimMonad m, Ord i) =>
            V.Vector (Nz i a) -> Int -> m (V.Vector (Nz i a))
sortOnIx v j = do
  vm <- V.thaw v
  VSM.sortBy (compareIx j) vm
  V.freeze vm

ptrV :: Int   -- ^ Index 
  -> Int32     -- ^ Dimensionality 
  -> V.Vector (Nz Int32 a)
  -> V.Vector Int32
ptrV j = csPtrV (ixUnsafe j)
  

-- | Given a number of rows(resp. columns) `n` and a _sorted_ Vector of Integers in increasing order (containing the column (resp. row) indices of nonzero entries), return the cumulative vector of nonzero entries of length `n` (the "column (resp. row) pointer" of the CSR(CSC) format). NB: Fused count-and-accumulate
-- E.g.:
-- > csPtrV 4 (V.fromList [1,1,2,3])
-- [0,2,3,4]
-- csPtrV :: Int -> Int32 -> V.Vector (Row Int32) -> V.Vector Int32
csPtrV :: (r -> Int32) -> Int32 -> V.Vector r -> V.Vector Int32
csPtrV ixf n xs = V.create createf where
  createf :: ST s (VM.MVector s Int32)
  createf = do
    let c = 0
    vm <- VM.new (fromIntegral n)
    VM.write vm 0 0  -- write `0` at position 0
    let loop v ll i count | i == n = return ()
                          | otherwise = do
                              let lp = V.length $ V.takeWhile (\r -> ixf r == i) ll
                                  count' = count + lp
                              VM.write v (fromIntegral i) (fromIntegral count')
                              loop v (V.drop lp ll) (succ i) count'
    loop vm xs 1 c
    return vm



-- -- | Given a number of rows(resp. columns) `n` and a _sorted_ Vector of Integers in increasing order (containing the column (resp. row) indices of nonzero entries), return the cumulative vector of nonzero entries of length `n` (the "column (resp. row) pointer" of the CSR(CSC) format). NB: Fused count-and-accumulate
-- -- E.g.:
-- -- > csPtrV 4 (V.fromList [1,1,2,3])
-- -- [0,2,3,4]
-- csPtrV :: Int32 -> V.Vector Int32 -> V.Vector Int32
-- csPtrV n xs = V.create createf where
--   createf :: ST s (VM.MVector s Int32)
--   createf = do
--     let c = 0
--     vm <- VM.new (fromIntegral n)
--     VM.write vm 0 0  -- write `0` at position 0
--     let loop v ll i count | i == n = return ()
--                           | otherwise = do
--                               let lp = V.length $ V.takeWhile (== i) ll
--                                   count' = count + lp
--                               VM.write v (fromIntegral i) (fromIntegral count')
--                               loop v (V.drop lp ll) (succ i) count'
--     loop vm xs 1 c
--     return vm




