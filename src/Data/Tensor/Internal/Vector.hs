{-# language TypeFamilies, FlexibleContexts, LambdaCase #-}
{-|
Module      : Data.Tensor.Internal.Vector
Description : Internal operations for operating on sparse tensors backed by dense vectors
Copyright   : (c) Marco Zocca, 2018
License     : GPL-3
Maintainer  : ocramz fripost org
Stability   : experimental
Portability : POSIX

Index sorting and compression routines for operating on sparse tensors backed by dense vectors
-}
module Data.Tensor.Internal.Vector (
  ptrV, sortOnIx
  -- * Internal
  , csPtrV
  ) where

-- import Data.Int (Int32)
-- import Data.Foldable (foldl')
import Data.Foldable (foldlM)
-- import Data.List (group, groupBy)
-- import qualified Data.Vector.Algorithms.Radix as VSR (sort, sortBy, Radix(..))
import qualified Data.Vector.Algorithms.Merge as VSM (sortBy)
import Data.Vector.Algorithms.Merge (Comparison)
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Mutable as VM
-- import qualified Data.IntMap as IM
import Control.Monad.Primitive
import Control.Monad.ST
-- import Data.Function (on)

import Data.Ord (comparing)
-- import qualified Data.List.NonEmpty as NE
-- import Prelude hiding ( (!!), length )
-- import Control.Parallel.Strategies (using, rpar, parTraversable)
import qualified Data.Tensor.Internal.Dim as D
import qualified Data.Tensor.Internal.Variance as DV

import Data.Tensor.Internal.Shape.Types

-- | A @Vector (Nz i a)@ contains the coordinate representation of the nonzero entries in a tensor.
--
-- The compressed-sparse-fiber (CSF) pointer vectors are computed by sorting this representation over one of its indices and counting repeated indices (with @ptrV@).
--
-- For example, the CSF computation for a rank-3 sparse tensor will entail 3 sorts and 3 corresponding calls of @ptrV@.
-- compressCOO ::
--   (Foldable t, PrimMonad m, COO r) =>
--      t (I, Ix, Bool, Bool) -- ^ (Index, Dimensionality, Dense dimension flag, Covariant dimension flag)
--   -> V.Vector r -- ^ Vector of tensor NZ elements in coordinate encoding.
--   -> m (V.Vector (COOEl r), DV.Var (D.DimE V.Vector Ix))

-- compressCOO ixs v0 = do
--   (vFinal, se) <- foldlM go (v0, DV.empty) ixs
--   pure (cooElem <$> vFinal, se)
--   where
--     go (v, se) (i, n, dense, covar) = do
--       v' <- sortOnIx v i
--       if not dense
--         then do 
--           let vp = ptrV i n v'
--               vi = ixCOO i <$> v'
--               sdim = D.sparseDimE vp vi n
--           pure (v', DV.insertWhen covar i sdim se)
--         else do
--           let ddim = D.denseDimE n
--           pure (v', DV.insertWhen covar i ddim se)




v1 = V.fromList [
    nz [0,0] 6
  , nz [2,0] 5
  , nz [0,2] 9
  , nz [0,3] 8
  , nz [2,3] 7
                ]

{- test data :

v1 = V.fromList [
    nz [0,0] 6
  , nz [2,0] 5
  , nz [0,2] 9
  , nz [0,3] 8
  , nz [2,3] 7  ]

λ> compressCOO [(0,3,False,True), (1,4,False,False)] v1
([6,5,9,8,7],Var {unVar = fromList [(Co 0,DimE {unDimE = Right (Sd {sPtr = [0,0,0], sIdx = [0,0,0,2,2], sDim = 3})}),(Contra 1,DimE {unDimE = Right (Sd {sPtr = [0,0,0,0], sIdx = [0,0,2,3,3], sDim = 4})})]})

-}

-- | Sort a vector of COOrdinate-encoded tensor elements along a given index
sortOnIx :: (PrimMonad m, COO coo) =>
            V.Vector coo  -- ^ vector of tensor entries in COOrdinate format
         -> I             -- ^ tensor index to sort on
         -> m (V.Vector coo)
sortOnIx v j = do
  vm <- V.thaw v
  VSM.sortBy (compareIxCOO j) vm
  V.freeze vm

-- | @pv = ptrV ix n vcoo@ computes the pointer vector @pv@ at a given index @ix@ along a dimension of given dimensionality @n@, from a vector of COOrdinate-encoded tensor elements @vcoo@
ptrV :: COO coo =>
        I   -- ^ Index 
     -> Ix     -- ^ Dimensionality 
     -> V.Vector coo -- ^ vector of tensor entries in COOrdinate format
     -> V.Vector Ix
ptrV j = csPtrV (ixCOO j)
  

-- | Given a number of rows(resp. columns) `n` and a _sorted_ Vector of Integers in increasing order (containing the column (resp. row) indices of nonzero entries), return the cumulative vector of nonzero entries of length `n` (the "column (resp. row) pointer" of the CSR(CSC) format). NB: Fused count-and-accumulate
-- E.g.:
-- > csPtrV 3 (V.fromList [0,0,1,2])
-- [0,2,3,4]
csPtrV :: (r -> Ix)  -- ^ Indexing function
       -> Ix         -- ^ Dimensionality
       -> V.Vector r -- ^ /Sorted/ vector of elements 
       -> V.Vector Ix
csPtrV ixf n xs = V.create createf where
  createf :: ST s (VM.MVector s Ix)
  createf = do
    let c = 0
    vm <- VM.new (fromIntegral $ n + 1)
    VM.write vm 0 0  -- write `0` at position 0
    let loop v ll i count | i == n = return ()
                          | otherwise = do
                              let lp = V.length $ V.takeWhile (\r -> ixf r == i) ll
                                  count' = count + lp
                              VM.write v (fromIntegral $ i + 1) (fromIntegral count')
                              loop v (V.drop lp ll) (succ i) count'
    loop vm xs 0 c
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
  | TCompWise (TExpr i a) (TExpr i a)  -- ^ Componentwise operations
  | TInner [P i] (TExpr i a) (TExpr i a) -- ^ Contraction (inner product)
  | TOuter [P i] (TExpr i a) (TExpr i a) -- ^ Exterior product
  deriving (Eq, Show)

-- contract i j = TInner [P i j]


-- | Pair of indices
data P i = P i i  deriving (Eq, Show)

-- -- | Index sets
-- data I i =
--     I1 i
--   | I2 i i
--   | I3 i i i   -- | ...
--   deriving (Eq, Show)



-- -- | Example usage : sparse vector
-- sv :: (PrimMonad m, Row r) =>
--       Ix -> V.Vector r -> m (V.Vector (REl r), D.DimsE V.Vector Ix)
-- sv m = compressCOO [(0, m, False, False)]

-- -- | Example usage : CSR sparse matrix
-- csr :: (PrimMonad m, Row r) =>
--        Ix
--     -> Ix
--     -> V.Vector r
--     -> m (V.Vector (REl r), D.DimsE V.Vector Ix)
-- csr m n = compressCOO [(0, m, True, False), (1, n, False, True)]







-- | playground

-- newtype Mu f = In { out :: f (Mu f)}


-- cata :: Functor f => (f b -> b) -> Mu f -> b
-- cata phi = phi . fmap (cata phi) . out

-- ana :: Functor f => (a -> f a) -> a -> Mu f
-- ana  psi = In  . fmap (ana  psi) . psi
