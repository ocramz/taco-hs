{-# language LambdaCase, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-|
Module      : Data.Dim
Description : Dimension data
Copyright   : (c) Marco Zocca, 2018
License     : GPL-3
Maintainer  : ocramz fripost org
Stability   : experimental
Portability : POSIX

This module contains types and construction/access functions for tensor dimension metadata which are declared to be polymorphic in the container type (i.e. could be lists, vectors etc.).
Note : no rank or dimensionality information is known at compile time, that is, size mismatch errors will have to be raised at runtime.
-}
module Data.Dim (
  -- * Variance annotation
    Variance(..), V(..), co, contra
  -- , coIx, contraIx
  -- -- ** Convenience constructors
  -- -- , mkVarVector, mkVarCoVector, mkVarMatrix    
  , empty, insert, rekey
  -- * Dimension metadata  
  , DimE(..), dimE, denseDimE, sparseDimE
  , Dd(..), Sd(..)
  ) where

-- import Control.Applicative hiding (empty)
-- import Data.Maybe (isJust)
-- import Control.Arrow ((***))
-- import Data.Int (Int32(..), Int64(..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.IntMap as IM
-- import qualified Data.Map.Strict as M

import Data.Shape.Types


{- 
Tensor product shorthand (Einstein notation) prescribes that only pairs of tensors with common indices can be multiplied. In particular, in the index pair one index should be variant and the other contravariant.
-}


newtype Variance v i = Variance { unVar :: Var (DimE v i) } deriving (Eq, Show)

instance Integral i => TShape (Variance v i) where
  tdim = getTDim . unVar

getTDim :: Integral i => Var (DimE v i) -> ([Int], [Int])
getTDim va = (gettd coIx, gettd contraIx) where
  gettd f = maybe [] toDims (f va)
  
empty :: Variance v i
empty = Variance emptyVar

rekey :: [IM.Key] -> Variance v i -> Variance v i
rekey ks (Variance v) = Variance $ rekeyVar ks v

insert :: IM.Key -> V (DimE v i) -> Variance v i -> Variance v i
insert k v (Variance va) = Variance $ insertVar k v va



-- | Variance annotation
data V a = Co a  -- ^ Covariant
  | Contra a  -- ^ Contravariant
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

co, contra :: a -> V a 
co = Co
contra = Contra

unV :: V a -> a
unV (Co x) = x
unV (Contra x) = x

newtype Var a =
  Var (IM.IntMap (V a)) deriving (Eq, Show, Functor, Foldable, Traversable)

emptyVar :: Var a
emptyVar = Var IM.empty

insertVar :: IM.Key -> V a -> Var a -> Var a
insertVar i di (Var m) = Var $ IM.insert i di m

-- mapKeysV :: (IM.Key -> IM.Key) -> Var a -> Var a
-- mapKeysV fk (Var im) = Var $ IM.mapKeys fk im

filterV :: (V a -> Bool) -> Var a -> Var a
filterV ff (Var im) = Var $ IM.filter ff im

filterMaybeV :: (V a -> Bool) -> Var a -> Maybe (Var a)
filterMaybeV q var
  | null va = Nothing
  | otherwise = Just va
  where
    va = filterV q var

-- | Get covariant indices
coIx :: Var a -> Maybe (Var a)
coIx = filterMaybeV isCo

-- | Get contravariant indices
contraIx :: Var a -> Maybe (Var a)
contraIx = filterMaybeV (not . isCo)

isCo :: V a -> Bool
isCo v = case v of Co _ -> True
                   _    -> False

toDims :: Integral i => Var (DimE v i) -> [Int]
toDims ne = (fromIntegral . dimE . unV) `map` toList ne

toList :: Var a -> [V a]
toList (Var im) = snd `map` IM.toList im

fromList :: [(IM.Key, V a)] -> Var a
fromList = Var . IM.fromList

-- | Discards the keys and applies a new set of keys supplied as the list argument
rekeyVar :: [IM.Key] -> Var a -> Var a
rekeyVar ixm (Var im) = fromList $ zip ixm vm
  where
    (_, vm) = unzip $ IM.toList im

intersectionWithKey
  :: (IM.Key -> V a1 -> V a2 -> V a3) -> Var a1 -> Var a2 -> Var a3
intersectionWithKey f (Var m1) (Var m2) = Var $ IM.intersectionWithKey f m1 m2


    





-- lookupDim :: DimsE v i -> IM.Key -> Maybe (DimE v i)
-- lookupDim im i = IM.lookup i (unDimsE im)





-- -- | The dimension metadata will be labeled in the list consumption order.
-- fromList :: [DimE v i] -> Maybe (DimsE v i)
-- fromList xs | null xs = Nothing
--             | otherwise = Just . DimsE $ IM.fromList $ zip [0 ..] xs







-- -- | A vector has a single contravariant index
-- mkVarVector :: DimE v i -> Maybe (Variance v i)
-- mkVarVector ixco = ContraVar <$> fromList [ixco]
-- -- | A co-vector has a single covariant index
-- mkVarCoVector :: DimE v i -> Maybe (Variance v i)
-- mkVarCoVector ixcontra = CoVar <$> fromList [ixcontra]
-- -- | A matrix has one covariant and one contravariant index
-- mkVarMatrix :: DimE v i -> DimE v i -> Maybe (Variance v i)
-- mkVarMatrix ixco ixcontra =
--   fmap Variance (BothVar <$> fromList [ixco] <*> fromList [ixcontra])




  
-- | Tensor dimensions can be either dense or sparse
newtype DimE v i = DimE {
  unDimE :: Either (Dd i) (Sd v i)
  } deriving (Eq)

dimE :: DimE v i -> i
dimE (DimE ei) = either dDim sDim ei

instance Show i => Show (DimE v i) where
  show (DimE ei) = either shd shs ei where
    shd (Dd n) = unwords ["D", show n]
    shs (Sd _ _ n) = unwords ["S", show n]

-- | Construct a dense DimE
denseDimE :: i -> DimE v i
denseDimE = DimE . Left . Dd 

-- | Construct a sparse DimE
sparseDimE :: v i -> v i -> i -> DimE v i
sparseDimE sv ixv n = DimE (Right (Sd sv ixv n))


-- | To define a /dense/ dimension we only need the dimensionality parameter (an integer)
newtype Dd i = Dd {
   -- | Dimensionality
    dDim :: i
  } deriving (Eq)

-- | To define a /sparse/ dimension we need a cumulative array, an index array and a dimensionality parameter
data Sd v i = Sd {
      -- | Location in the sIdx array where each segment begins. 
      sPtr :: v i
      -- | Index array (indices of nonzero entries)
    , sIdx :: v i
      -- | Dimensionality 
    , sDim :: i
    } deriving (Eq)

instance Show i => Show (Dd i) where
  show (Dd n) = unwords ["D", show n]

instance Show i => Show (Sd v i) where
  show (Sd _ _ sdim) = unwords ["S", show sdim]






  
  
