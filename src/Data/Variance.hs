{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, LambdaCase #-}
module Data.Variance where

import Data.Dim

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM

import Control.Arrow (Arrow(..), (***), (&&&))

import Data.Shape.Types

both :: Arrow a => a b' c' -> a (b', b') (c', c')
both f = f *** f


-- | A variance annotation for tensor indices
data V = Co !Int | Contra !Int deriving (Eq, Show)

eitherVar :: (Int -> p) -> V -> p
eitherVar f = \case
  Co i -> f i
  Contra i -> f i

unV :: V -> Int
unV = eitherVar id

liftV2 :: (Int -> Int -> t) -> V -> V -> t
liftV2 f v1 v2 = f (unV v1) (unV v2)

-- | This Ord instance only compares the Int content of the constructors
instance Ord V where
  compare = liftV2 compare

partitionV :: Foldable t => t V -> ([V], [V])
partitionV va = foldr ins ([], []) va where
  ins v (l, r) = case v of 
    x@(Co _)     -> (x : l, r)
    y@(Contra _) -> (l, y : r)


-- | A tensor variance annotation can be encoded by a Map that is keyed by 'V's (i.e. each index can be either co- or contravariant)
newtype Var a = Var (M.Map V a) deriving (Eq, Show, Functor, Foldable, Traversable)

fromList :: [(V, a)] -> Var a
fromList = Var . M.fromList

toList :: Var a -> [(V, a)]
toList (Var im) = M.toList im

-- | Dimensionality of each index
toDims :: Integral i => Var (DimE v i) -> [Int]
toDims ne = (fromIntegral . dimE . snd) `map` toList ne



-- | A tensor variance annotation using 'DimE' as metadata
newtype Variance v i = Variance { unVar :: Var (DimE v i) } deriving (Eq, Show)

instance Integral i => TShape (Variance v i) where
    -- tdim = getTDim . unVar


getTDim :: Foldable t => t V -> ([Int], [Int])
getTDim va = both (map unV) $ partitionV va

-- empty :: Variance v i
-- empty = Variance emptyVar

-- rekey :: [IM.Key] -> Variance v i -> Variance v i
-- rekey ks (Variance v) = Variance $ rekeyVar ks v

-- insert :: IM.Key -> V (DimE v i) -> Variance v i -> Variance v i
-- insert k v (Variance va) = Variance $ insertVar k v va





-- -- | An IntMap of things that can be either covariant or contravariant
-- newtype Var a =
--   Var (IM.IntMap (V a)) deriving (Eq, Show, Functor, Foldable, Traversable)

-- lookupCo :: IM.Key -> Var b -> Maybe b
-- lookupCo k (Var im) = IM.lookup k im >>= getCo

-- lookupContra :: IM.Key -> Var b -> Maybe b
-- lookupContra k (Var im) = IM.lookup k im >>= getContra

-- -- | Variance annotation
-- data V a = Co a  -- ^ Covariant
--   | Contra a  -- ^ Contravariant
--   deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- co, contra :: a -> V a 
-- co = Co
-- contra = Contra

-- unV :: V a -> a
-- unV (Co x) = x
-- unV (Contra x) = x

-- getCo :: V a -> Maybe a
-- getCo (Co x) = Just x
-- getCo _ = Nothing

-- getContra :: V a -> Maybe a
-- getContra (Contra x) = Just x
-- getContra _ = Nothing

-- emptyVar :: Var a
-- emptyVar = Var IM.empty

-- insertVar :: IM.Key -> V a -> Var a -> Var a
-- insertVar i di (Var m) = Var $ IM.insert i di m

-- -- mapKeysV :: (IM.Key -> IM.Key) -> Var a -> Var a
-- -- mapKeysV fk (Var im) = Var $ IM.mapKeys fk im


-- filterV ff (Var im) = Var $ M.filter ff im


-- filterMaybeV :: (V a -> Bool) -> Var a -> Maybe (Var a)
-- filterMaybeV q var
--   | null va = Nothing
--   | otherwise = Just va
--   where
--     va = filterV q var

-- -- | Get covariant indices
-- coIx :: Var a -> Maybe (Var a)
-- coIx = filterMaybeV isCo

-- -- | Get contravariant indices
-- contraIx :: Var a -> Maybe (Var a)
-- contraIx = filterMaybeV (not . isCo)

-- isCo :: V a -> Bool
-- isCo v = case v of Co _ -> True
--                    _    -> False






-- -- | Discards the keys and applies a new set of keys supplied as the list argument
-- rekeyVar :: [IM.Key] -> Var a -> Var a
-- rekeyVar ixm (Var im) = fromList $ zip ixm vm
--   where
--     (_, vm) = unzip $ IM.toList im

-- rekey0 :: Var a -> Var a
-- rekey0 = rekeyVar [0 .. ]    

-- intersectionWithKey
--   :: (IM.Key -> V a1 -> V a2 -> V a3) -> Var a1 -> Var a2 -> Var a3
-- intersectionWithKey f (Var m1) (Var m2) = Var $ IM.intersectionWithKey f m1 m2


    





