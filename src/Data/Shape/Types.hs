{-# language TypeOperators, TypeFamilies #-}
module Data.Shape.Types where

import Control.Arrow 

data Z
data sh :# e -- dense
data sh :. e -- sparse


-- | A class for data that have a shape.
class Shape t where
  -- type ShapeT t :: *
  -- shape :: t -> ShapeT t
  rank :: t -> Int
  dim :: t -> [Int]


class TShape t where
  tdim :: t -> ([Int], [Int])

trank :: TShape t => t -> (Int, Int)
trank tt = (length co , length contra) where
  (co, contra) = tdim tt
