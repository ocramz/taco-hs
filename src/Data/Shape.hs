{-# language GADTs, TypeOperators #-}

module Data.Shape where

import GHC.TypeLits


-- data Z
-- data sh :. e

-- data Shape sh where
--   Z :: Shape Z
--   (:.) :: Shape sh -> i -> Shape (sh :. i)

-- dimz :: Shape sh -> Int
-- dimz Z = 0
-- dimz (sh :. _) = dimz sh + 1




-- | An index of dimension zero
data Z  = Z
        deriving (Show, Read, Eq, Ord)

-- | Our index type, used for both shapes and indices.
infixl 3 :.
data tail :. head = !tail :. !head
        deriving (Show, Read, Eq, Ord)



class Eq sh => Shape sh where
  rank :: sh -> Int
  listOfShape :: sh -> [Int]
  shapeOfList :: [Int] -> sh
