module Data.Shape.Dynamic where

import Data.Foldable (foldl')

import qualified Data.Dim as Dim

newtype Sh i =
    Sh {unShD :: [Either (Dim.Dd i) (Dim.Sd i)] }
      deriving (Eq, Show)

mkDenseSh :: Foldable t => t i -> Sh i
mkDenseSh xss = Sh $ foldl' (\d s -> Left (Dim.Dd s) : d) [] xss

rank :: Sh i -> Int
rank = length . unShD 

-- dim :: ShD i -> [i]
dim :: (Num b, Integral a) => Sh a -> [b]
dim sh = fromIntegral <$> foldl' (\d s -> Dim.dim s : d) [] (unShD sh)



