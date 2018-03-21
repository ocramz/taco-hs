module Data.Shape.Dynamic.Named where

import Data.Foldable (foldl')
import qualified Data.Map.Lazy as M
import qualified Data.Set as S

-- import qualified Data.Dim.Named as Dim
import qualified Data.Dim.Generic as DG
import Data.Shape.Types


newtype Sh n v i =
  Sh {
    unShDn :: M.Map n (Either (DG.Ddn i) (DG.Sdn v i))
    } deriving (Eq, Show)

-- | Construct a shape given a list of either dense of sparse dimensions
mkShDn :: Ord n => 
     [(n, Either (DG.Ddn i) (DG.Sdn v i))] -> Sh n v i
mkShDn = Sh . M.fromList

ixLabels :: Ord n => Sh n v i -> S.Set n
ixLabels = S.fromList . M.keys . unShDn

rank_ :: Sh n v i -> Int
rank_ = length . unShDn

dim_ :: Integral i => Sh n v i -> [Int]
dim_ sh = fromIntegral . DG.dim . snd <$> M.toList (unShDn sh)


-- | Combinator that acts on the intersection of two index sets. If all matching indices have same dimensions, return some function of the corresponding dimension pair.
shDiff :: (Eq a, Applicative f, Ord k) =>
     (Maybe
        (Either (DG.Ddn a) (DG.Sdn v a), Either (DG.Ddn a) (DG.Sdn v a))
      -> f b)
     -> Sh k v a
     -> Sh k v a
       -> f (M.Map k b)
shDiff g sh1 sh2 =
  traverse g $ M.intersectionWith f (unShDn sh1) (unShDn sh2) where
    f a b | DG.dim a == DG.dim b = Just (a, b)
          | otherwise = Nothing


instance Integral i => Shape (Sh n v i) where
  rank = rank_
  dim = dim_



-- newtype ShDn n i =
--   ShDn {
--     unShDn :: [Either (Dim.Ddn i n) (Dim.Sdn i n)]
--     } deriving (Eq, Show)

-- mkDenseShDn :: Foldable t => t (n, i) -> ShDn n i
-- mkDenseShDn xss = ShDn $ foldr insf [] xss  where
--   insf (x, ixname) acc = Left (Dim.Ddn x ixname) : acc

-- rank :: ShDn n i -> Int
-- rank = length . unShDn

-- dim :: (Num b, Integral a) => ShDn a n2 -> [b]
-- dim sh = fromIntegral <$> foldl' (\d s -> Dim.dim s : d) [] (unShDn sh)
