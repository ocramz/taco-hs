{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, LambdaCase #-}
module Data.Tensor.Internal.Variance
  -- (
  -- I,
  -- V(..),
  -- Var(..), empty, insertWhen,
  -- Variance(..)
  -- )
  where

-- import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
-- import qualified Data.IntMap.Strict as IM

import Control.Arrow (Arrow(..), (***))

import Data.Tensor.Internal.Dim
import Data.Tensor.Internal.Shape.Types


-- | A tensor variance annotation using 'DimE' as metadata
newtype Variance v i = Variance { unVariance :: Var (DimE v i) } deriving (Eq, Show)

-- instance Integral i => TShape (Variance v i) where
--     tdim = getTDim 




-- | A tensor variance annotation can be encoded by a Map that is keyed by 'V's (i.e. each index can be either co- or contravariant)
newtype Var a = Var { unVar :: M.Map V [a] } deriving (Eq, Show, Functor, Foldable, Traversable)

-- fromList :: [(V, a)] -> Var a
-- fromList = Var . M.fromList

consVar :: V -> a -> Var a -> Var a
consVar k v (Var mm) = Var $ M.insertWith (<>) k [v] mm 

consVarWhen :: Bool -> I -> a -> Var a -> Var a
consVarWhen flag ki v mm = consVar k v mm where
  k | flag = Co ki
    | otherwise = Contra ki

insert :: V -> [a] -> Var a -> Var a
insert k v (Var mm) = Var $ M.insert k v mm

insertWhen :: Bool -> I -> [a] -> Var a -> Var a
insertWhen flag ki v mm = insert k v mm where
  k | flag = Co ki
    | otherwise = Contra ki

empty :: Var a
empty = Var M.empty    

-- toList :: Var a -> [(V, a)]
-- toList (Var im) = M.toList im



-- | A variance annotation for tensor indices
data V = Co !I | Contra !I deriving (Eq, Show)

eitherVar :: (I -> p) -> V -> p
eitherVar f = \case
  Co i -> f i
  Contra i -> f i

unV :: V -> I
unV = eitherVar id

liftV2 :: (I -> I -> t) -> V -> V -> t
liftV2 f v1 v2 = f (unV v1) (unV v2)

-- | This Ord instance only compares the Int content of the constructors
instance Ord V where
  compare = liftV2 compare








-- | Promote an arrow to an arrow between homogeneous tuples
both :: Arrow a => a b' c' -> a (b', b') (c', c')
both f = f *** f


 
