{-# language GADTs, TypeOperators #-}
module Data.Shape.Named where

import Data.Monoid ((<>))
import Data.Int (Int32)
import qualified Data.Vector.Unboxed as VU

import qualified Data.Dim.Named as Dim
import Data.Shape.Types


-- | A Shape, for which each dimension is tagged by a name
data Sh n sh where
  Z :: Sh n Z
  D :: Dim.Ddn n Int32 -> Sh n sh -> Sh n (sh :# Int32)
  S :: Dim.Sdn n Int32 -> Sh n sh -> Sh n (sh :. Int32)

instance Show n => Show (Sh n sh) where
  show Z = ""
  show (D (Dim.Ddn m ixn) sh) = unwords ["D", show ixn, show m, show sh]
  show (S (Dim.Sdn _ ix nd ixn) sh) = "S " <> show ixn <> showSparse ix nd <> show sh where
    showSparse ixx nn = show (VU.length ixx, nn)
  
ixes :: Sh n sh -> [n]
ixes Z = []
ixes (D d sh) = Dim.dnIx d : ixes sh
ixes (S d sh) = Dim.snIx d : ixes sh

-- | Does the element belong to the index set of the shape?
elemIx :: Eq a => a -> Sh a sh -> Bool
elemIx i sh = i `elem` ixes sh





-- -- test data

data Ix = I | J | K deriving (Eq, Show)

-- ix0 :: Int32 -> Sh Char (Z :# Int32)
ix0 = D (Dim.Ddn 2 I ) Z

-- ix1 :: Int32 -> Int32 -> Sh Char ((Z :# Int32) :# Int32)
ix1 = D (Dim.Ddn 2 I) (D (Dim.Ddn 3 J) Z)

