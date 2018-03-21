{-# language TypeOperators, TypeFamilies #-}
module Data.Shape.Types where

data Z
data sh :# e -- dense
data sh :. e -- sparse
