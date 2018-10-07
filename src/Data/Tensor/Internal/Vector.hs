{-# language TypeFamilies, FlexibleContexts, LambdaCase #-}
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
import qualified Data.Dim as D


-- | Here we fix the size of the address space.
--
-- @type Ix = Int32@
type Ix = Int32

-- | Row types that can be indexed via an integer parameter
class COO r where
  type COOEl r :: *
  ixCOO :: Int -> r -> Ix
  mkCOO :: [Ix] -> COOEl r -> r
  cooElem :: r -> COOEl r

instance COO (Nz a) where
  type COOEl (Nz a) = a
  ixCOO = ixUnsafe
  mkCOO = fromList
  cooElem = nzEl

compareIxRow :: COO r => Int -> r -> r -> Ordering
compareIxRow j = comparing (ixCOO j)  


-- | A nonzero element in coordinate form
data Nz a = Nz {
    nzIxs :: !(NE.NonEmpty Ix)
  , nzEl :: a } deriving (Eq, Show)

fromList :: [Ix] -> a -> Nz a
fromList = Nz . NE.fromList


-- | Unsafe : it assumes the index is between 0 and (length - 1)
ixUnsafe :: Int -> Nz a -> Ix
ixUnsafe i (Nz ne _) = ne NE.!! i  

-- ixUnsafeTup3 :: Int -> (Ix, Ix, Ix) -> Ix
-- ixUnsafeTup3 i (a, b, c) = case i of
--   0 -> a
--   1 -> b
--   2 -> c
--   _ -> error "derp"

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
compressCOO :: (PrimMonad m, Foldable t, COO coo) =>
               t (Int, Ix, Bool, Bool) -- ^ (Index, Dimensionality, Dense dimension flag, Covariant dimension flag)
            -> V.Vector coo
            -> m (V.Vector (COOEl coo), D.Variance V.Vector Ix)
compressCOO ixs v0 = do 
  (vFinal, se) <- foldlM go (v0, D.empty) ixs
  pure (cooElem <$> vFinal, se)
  where
    go (v, se) (i, n, dense, covar) = do
      v' <- sortOnIx v i
      if not dense
        then do 
          let vp = ptrV i n v'
              vi = ixCOO i <$> v'
              sdim = D.sparseDimE vp vi n
              sdimv | covar = D.co sdim
                    | otherwise = D.contra sdim
          pure (v', D.insert i sdimv se)
        else do
          let ddim = D.denseDimE n
              ddimv | covar = D.co ddim
                    | otherwise = D.contra ddim
          pure (v', D.insert i ddimv se)


sortOnIx :: (PrimMonad m, COO coo) =>
            V.Vector coo -> Int -> m (V.Vector coo)
sortOnIx v j = do
  vm <- V.thaw v
  VSM.sortBy (compareIxRow j) vm
  V.freeze vm

ptrV :: COO coo =>
        Int   -- ^ Index 
     -> Ix     -- ^ Dimensionality 
     -> V.Vector coo
     -> V.Vector Ix
ptrV j = csPtrV (ixCOO j)
  

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








{-|
Tensor expressions are constructed as binary syntax trees.

-}

data Expr a =
    Const a
  | Plus (Expr a) (Expr a)
  | Times (Expr a) (Expr a)
  deriving (Eq, Show)


k :: a -> Expr a
k = Const

plus, times :: Expr a -> Expr a -> Expr a
plus = Plus
times = Times

eval :: Num a => Expr a -> a
eval = \case
  Const x -> x
  Plus e1 e2 -> eval e1 + eval e2
  Times e1 e2 -> eval e1 * eval e2


-- --


data TExpr i a =
    TConst a
  | TContract i i (TExpr i a) (TExpr i a)
  deriving (Eq, Show)

tcontract = TContract


-- te0 t1 t2 t3 = TContract 2 t1 (TContract 3 t2 t3)





-- -- | Example usage : sparse vector
-- sv :: (PrimMonad m, Row r) =>
--       Ix -> V.Vector r -> m (V.Vector (REl r), D.DimsE V.Vector Ix)
-- sv m = compressCOO [(0, m, False)]

-- -- | Example usage : CSR sparse matrix
-- csr :: (PrimMonad m, Row r) =>
--        Ix
--     -> Ix
--     -> V.Vector r
--     -> m (V.Vector (REl r), D.DimsE V.Vector Ix)
-- csr m n = compressCOO [(0, m, True), (1, n, False)]
