module Data.Shape.Dynamic where

import qualified Data.Dim as Dim


newtype ShD i =
    ShD {unShD :: [Either (Dim.Dd i) (Dim.Sd i)] }
      deriving (Eq, Show)



