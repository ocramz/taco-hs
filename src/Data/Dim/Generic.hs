{-# language DeriveFunctor, UndecidableInstances #-}
{-|
Module      : Data.Dim.Generic
Description : Dimension data, parametrized by an arbitrary container type
Copyright   : (c) Marco Zocca, 2018
License     : GPL-3
Maintainer  : ocramz fripost org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module Data.Dim.Generic where

import Data.Int (Int32(..), Int64(..))

-- import qualified Data.IntMap.Strict as M
import Data.List.NonEmpty (NonEmpty(..), fromList, toList)

import Data.Shape.Types

-- | Dimension metadata, stored into an 'IntMap' of 'DimE'.
--
-- The IntMap storage means that index metadata for an arbitrary dimension can be retrieved by name in logarithmic time.
-- newtype DimsE v e = DimsE {
--   unDimsE :: M.IntMap (DimE v e)
--   } deriving (Eq, Show, Functor)

-- instance Shape (DimsE v e) where
--   dim (DimsE de) = map (dimE . snd) $ M.toList de 
--   rank = product . dim




-- | Variance annotation
data Variance v i =
  CoVar (NonEmpty (DimE v i)) -- ^ Only covariant indices
  | ContraVar (NonEmpty (DimE v i)) -- ^ Only contravariant indices
  | BothVar (NonEmpty (DimE v i)) (NonEmpty (DimE v i)) -- ^ Both variant and contravariant indices
  deriving (Eq, Show)

-- -- | Semantic function for 'Variance' metadata (like 'either' for 'Either')
-- variance :: (NonEmpty (DimE v) -> p)
--          -> (NonEmpty (DimE v) -> p)
--          -> (NonEmpty (DimE v) -> NonEmpty (DimE v) -> p)
--          -> Variance v
--          -> p
-- variance f g h v = case v of
--   CoVar n -> f n
--   ContraVar n -> g n
--   BothVar m n -> h m n

-- instance Eq (v Int) => Eq (Variance v) where
--   CoVar v1 == CoVar v2 = v1 == v2
--   ContraVar v1 == ContraVar v2 = v1 == v2
--   BothVar u1 v1 == BothVar u2 v2 = u1 == u2 && v1 == v2
--   _ == _ = False

-- mkVector :: DimE v -> Variance v
-- mkVector ixco = CoVar (fromList [ixco])
-- mkCoVector :: DimE v -> Variance v
-- mkCoVector ixcontra = ContraVar (fromList [ixcontra])
-- mkMatrix :: DimE v -> DimE v -> Variance v
-- mkMatrix ixco ixcontra = BothVar (fromList [ixco]) (fromList [ixcontra])

-- instance TShape (Variance v i) where
--   tdim sh = case sh of
--     CoVar ne -> (toDims ne, [])
--     ContraVar ne -> ([], toDims ne)
--     BothVar neco necontra -> (toDims neco, toDims necontra)

-- toDims :: NonEmpty (DimE v) -> [Int]
toDims ne = dimE `map` toList ne

-- | Contraction indices
newtype CIx = CIx (NonEmpty Int) deriving (Eq, Show)

  
-- | Tensor dimensions can be either dense or sparse
newtype DimE v i = DimE {
  unDimE :: Either (Dd i) (Sd v i)
  } deriving (Eq)

dimE :: DimE v i -> i
dimE (DimE ei) = either dDim sDim ei

instance Show i => Show (DimE v i) where
  show (DimE ei) = either shd shs ei where
    shd (Dd n) = unwords ["dense :", show n]
    shs (Sd _ _ n) = unwords ["sparse :", show n]

-- | Construct a dense DimE
-- denseDimE :: Int  -> DimE v i
denseDimE = DimE . Left . Dd 

-- | Construct a sparse DimE
-- sparseDimE :: Maybe (v Int) -> v Int -> Int -> DimE v
sparseDimE sv ixv n = DimE (Right (Sd sv ixv n))


-- | To define a /dense/ dimension we only need the dimensionality parameter (an integer)
newtype Dd i = Dd {
   -- | Dimensionality
    dDim :: i
  } deriving (Eq)

-- | To define a /sparse/ dimension we need a cumulative array, an index array and a dimensionality parameter
data Sd v i = Sd {
      -- | Location in the sIdx array where each segment begins. Not all storage formats (e.g. COO for rank-2 tensors) need this information, hence it's wrapped in a Maybe.
      sPtr :: Maybe (v i)
      -- | Index array (indices of nonzero entries)
    , sIdx :: v i
      -- | Dimensionality 
    , sDim :: i
    } deriving (Eq)

-- instance Eq (v Int) => Eq (Sd v) where
--   Sd p1 i1 d1 == Sd p2 i2 d2 = p1 == p2 && i1 == i2 && d1 == d2

-- dim :: Integral i => Either (Dd i) (Sd v i) -> Int
-- dim = fromIntegral . either dDim sDim 

instance Show i => Show (Dd i) where
  show (Dd n) = unwords ["D", show n]

instance Show i => Show (Sd v i) where
  show (Sd _ _ sdim) = unwords ["S", show sdim]

