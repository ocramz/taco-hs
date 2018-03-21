module Data.Shape.Dynamic.Named where

import Data.Foldable (foldl')

import qualified Data.Dim.Named as Dim

newtype ShDn n i =
  ShDn {
    unShDn :: [Either (Dim.Ddn i n) (Dim.Sdn i n)]
    } deriving (Eq, Show)

mkDenseShDn :: Foldable t => t (n, i) -> ShDn n i
mkDenseShDn xss = ShDn $ foldr insf [] xss  where
  insf (x, ixname) acc = Left (Dim.Ddn x ixname) : acc

rank :: ShDn n i -> Int
rank = length . unShDn

dim :: (Num b, Integral a) => ShDn a n2 -> [b]
dim sh = fromIntegral <$> foldl' (\d s -> Dim.dim s : d) [] (unShDn sh)
