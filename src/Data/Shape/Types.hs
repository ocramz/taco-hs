{-# language TypeOperators, TypeFamilies #-}
module Data.Shape.Types where

data Z
data sh :# e -- dense
data sh :. e -- sparse


-- | A class for data that have a shape.
class Shape t where
  type ShapeT t :: *
  shape :: t -> ShapeT t
  shRank :: t -> Int
  shDim :: t -> [Int]
