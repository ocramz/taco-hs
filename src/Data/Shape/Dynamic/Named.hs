module Data.Shape.Dynamic.Named where

import Data.Foldable (foldl')

import qualified Data.Dim.Named as Dim
import qualified Data.Dim.Named.Generic as DG
import Data.Shape.Types

newtype ShDn n v i =
  ShDn {
    unShDn :: [Either (DG.Ddn n i) (DG.Sdn n v i)]
    } deriving (Eq, Show)

-- | Construct a shape given a list of either dense of sparse dimensions
mkShDn :: Foldable t =>
     t (Either (DG.Ddn n i) (DG.Sdn n v i)) -> ShDn n v i
mkShDn xss = ShDn $ foldr (:) [] xss

mkDenseShDn :: Foldable t => t (i, n) -> ShDn n v i
mkDenseShDn xss = ShDn $ foldr insf [] xss  where
  insf (x, ixname) acc = Left (DG.Ddn x ixname) : acc

rank_ :: ShDn n v i -> Int
rank_ = length . unShDn

dim_ :: (Num b, Integral i) => ShDn n v i -> [b]
dim_ sh = fromIntegral <$> foldl' (\d s -> DG.dim s : d) [] (unShDn sh)


instance Integral i => Shape (ShDn n v i) where
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
