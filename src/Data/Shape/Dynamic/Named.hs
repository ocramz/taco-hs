{-# language PackageImports #-}
{-|
Module      : Data.Shape.Dynamic.Named
Description : Labelled tensor shape (i.e. index) data
Copyright   : (c) Marco Zocca, 2018
License     : GPL-3
Maintainer  : ocramz fripost org
Stability   : experimental
Portability : POSIX

This module defines a notion of tensor shape where each dimension is of dynamically-checked type (i.e. 'Either' dense or sparse) and has a name (which could be a string or other symbol).

commentary with @some markup@.
-}
module Data.Shape.Dynamic.Named where

import Data.Foldable (foldl')
import qualified Data.Map.Lazy as M
import qualified Data.Set as S

import Control.Exception
import "exceptions" Control.Monad.Catch (MonadThrow(..), throwM)

-- import qualified Data.Dim.Named as Dim
import qualified Data.Dim.Generic as DG
-- import Data.Dim.Types
import Data.Shape.Types
import Data.Tensor.Exception



-- | A shape Sh is a finite map from indices to either a dense or a sparse dimension
newtype Sh n v i =
  Sh {
    unShDn :: M.Map n (DG.DimE v i)
    } deriving (Eq)

-- instance (Show n, Show i, Show (v i)) => Show (Sh n v i) where
--   show (Sh m) = show $ M.toList m

-- instance Integral i => Shape (Sh n v i) where
--   rank = rank_
--   dim = dim_
  

-- -- | Construct a shape given a list of either dense of sparse dimensions
-- mkSh :: Ord n => [(n, DimGE v i)] -> Sh n v i
-- mkSh = Sh . M.fromList

-- -- | Construct a shape of dense dimensions given a list of index labels and a list of dimensionalities
-- mkShD :: Ord n => [n] -> [i] -> Sh n v i
-- mkShD ixs xs = Sh (M.fromList $ zip ixs (map (Left . DG.Dd) xs))

-- rank_ :: Sh n v i -> Int
-- rank_ = length . unShDn

-- dim_ :: Integral i => Sh n v i -> [Int]
-- dim_ sh = fromIntegral . DG.dim . snd <$> M.toList (unShDn sh)


-- -- | Combinator that acts on the intersection of two index sets. If all matching indices have same dimensions, return some function of the corresponding dimension pair, otherwise throws a (pure) exception, e.g. returns Nothing or Left (MismatchedDims .. ).
-- --
-- -- This is meant to be used to represent tensor contractions; in the case of tensor contraction, the two shape operands represent the covariant index set of the first tensor and the contravariant i.s. of the second one.
-- shDiff :: (MonadThrow m, Ord k, Integral i) =>
--      (DimGE v i -> DimGE v i -> b)
--      -> Sh k v i -> Sh k v i -> m (M.Map k b)
-- shDiff h sh1 sh2 = sequence $ M.intersectionWith f (unShDn sh1) (unShDn sh2) where
--   f a b = let da = DG.dim a
--               db = DG.dim b
--           in
--             if da == db then pure (h a b) else throwM (MismatchedDims da db)





