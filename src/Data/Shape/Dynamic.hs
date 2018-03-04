module Data.Shape.Dynamic where

import Data.Foldable (foldl')

import qualified Data.Dim as Dim


newtype ShD i =
    ShD {unShD :: [Either (Dim.Dd i) (Dim.Sd i)] }
      deriving (Eq, Show)

mkDenseShD :: Foldable t => t i -> ShD i
mkDenseShD xss = ShD $ foldl' (\d s -> Left (Dim.Dd s) : d) [] xss

rank :: ShD i -> Int
rank = length . unShD 

-- dim :: ShD i -> [i]
dim :: (Num b, Integral a) => ShD a -> [b]
dim sh = fromIntegral <$> foldl' (\d s -> Dim.dim s : d) [] (unShD sh)



