{-# language TypeFamilies, FlexibleContexts #-}
module Data.Tensor.Internal.Vector where

import Data.Int (Int32)
-- import Data.Foldable (foldl')
import Data.Foldable (foldlM)
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

-- import Control.Parallel.Strategies (using, rpar, parTraversable)

import Data.Dim


-- | I think it's a good idea to fix the type of the address space. For now it's set here as 'Int32'.
type Ix = Int32

-- | Row types that can be indexed via an integer parameter
class Row r where
  type REl r :: *
  ixRow :: Int -> r -> Ix
  mkRow :: [Ix] -> REl r -> r
  rowElem :: r -> REl r

instance Row (Nz a) where
  type REl (Nz a) = a
  ixRow = ixUnsafe
  mkRow = fromList
  rowElem = nzEl

compareIxRow :: Row r => Int -> r -> r -> Ordering
compareIxRow j = comparing (ixRow j)  


-- | A nonzero element in coordinate form
data Nz a = Nz {
    nzIxs :: !(NE.NonEmpty Ix)
  , nzEl :: a } deriving (Eq, Show)

fromList :: [Ix] -> a -> Nz a
fromList = Nz . NE.fromList


-- | Unsafe : it assumes the index is between 0 and (length - 1)
ixUnsafe :: Int -> Nz a -> Ix
ixUnsafe i (Nz ne _) = ne NE.!! i  

-- | Unsafe : it assumes the index is between 0 and (length - 1)
compareIx :: Int -> Nz a -> Nz a -> Ordering
compareIx i = comparing (ixUnsafe i)


-- | A @Vector (Nz i a)@ contains the coordinate representation of the nonzero entries in a tensor.
--
-- The compressed-sparse-fiber (CSF) pointer vectors are computed by sorting this representation over one of its indices and counting repeated indices (with @ptrV@).
--
-- For example, the CSF computation for a rank-3 sparse tensor will entail 3 sorts and 3 corresponding calls of @ptrV@.
--
-- In this implementation, we use parallel strategies to evaluate in parallel the sort-and-count.
compressCOO :: (Foldable t, PrimMonad m, Row r) =>
               t (Int, Ix, Bool)
            -> V.Vector r
            -> m (V.Vector (REl r), [Either (V.Vector Ix) Ix])
compressCOO ixs v0 = do 
  (St vFinal se) <- foldlM go (St v0 []) ixs
  pure (rowElem <$> vFinal, se)
  where
    go (St v se) (i, n, dense) = do
      v' <- sortOnIx v i
      if not dense
        then
        do 
          let vp = ptrV i n v'
              se' = Left vp : se
          pure (St v' se')
        else pure (St v' (Right n : se))

data St a = St {
    stv :: V.Vector a
  , ste :: [Either (V.Vector Ix) Ix]
  } deriving (Eq, Show)


-- | Example usage
csr :: (PrimMonad m, Row r) =>
       Ix
    -> Ix
    -> V.Vector r
    -> m (V.Vector (REl r), [Either (V.Vector Ix) Ix])
csr m n = compressCOO [(0, m, True), (1, n, False)]


sortOnIx :: (PrimMonad m, Row r) =>
            V.Vector r -> Int -> m (V.Vector r)
sortOnIx v j = do
  vm <- V.thaw v
  VSM.sortBy (compareIxRow j) vm
  V.freeze vm

ptrV :: Row r =>
        Int   -- ^ Index 
     -> Ix     -- ^ Dimensionality 
     -> V.Vector r
     -> V.Vector Ix
ptrV j = csPtrV (ixRow j)
  

-- | Given a number of rows(resp. columns) `n` and a _sorted_ Vector of Integers in increasing order (containing the column (resp. row) indices of nonzero entries), return the cumulative vector of nonzero entries of length `n` (the "column (resp. row) pointer" of the CSR(CSC) format). NB: Fused count-and-accumulate
-- E.g.:
-- > csPtrV 4 (V.fromList [1,1,2,3])
-- [0,2,3,4]
-- csPtrV :: Int -> Int32 -> V.Vector (Row Int32) -> V.Vector Int32
csPtrV :: (r -> Ix) -> Ix -> V.Vector r -> V.Vector Ix
csPtrV ixf n xs = V.create createf where
  createf :: ST s (VM.MVector s Ix)
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



