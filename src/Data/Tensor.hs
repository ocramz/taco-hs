{-# language GADTs #-}
-- {-# language DeriveFunctor #-}
{-# language TypeOperators #-}
-- {-# language PackageImports #-}
{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}
module Data.Tensor
  -- (
  -- -- * Tensor type
  -- Tensor(..),
  -- tshape, tdata, nnz, rank, dim, 
  -- -- * Shape type
  -- Sh(..),
  -- -- * Dimension types
  -- Dim.Dd(..), Dim.Sd(..)
  -- )
  where

-- import qualified Data.Vector as V
-- import qualified Data.Vector.Unboxed as VU
-- import Data.Int (Int32)
import Control.Applicative
import qualified Data.Set as S

import Data.Shape.Types (Shape(..), rank, dim, Z, (:#), (:.))
import Data.Shape.Dynamic.Named (Sh(..), ixLabels)
import qualified Data.Dim.Generic (Ddn(..), Sdn(..))


-- | Covariant indices, contravariant indices, container type, element type
data Tensor i j v e where
  Tensor ::    
       Sh n v i                    
    -> Sh n v i                    
    -> v e  
    -> Tensor (Sh n v i) (Sh n v i) v e    

mkTensor ::
  Sh n v i -> Sh n v i -> v e -> Tensor (Sh n v i) (Sh n v i) v e
mkTensor = Tensor

instance Functor v => Functor (Tensor i j v) where
  fmap f (Tensor shi shj v) = Tensor shi shj (f <$> v)

-- | Covariant indices
coIx :: Tensor i j v e -> i
coIx (Tensor ix _ _) = ix

-- | Contravariant indices
contraIx :: Tensor i j v e -> j
contraIx (Tensor _ ix _) = ix

-- | Two tensors can be contracted if some covariant indices in the first appear in the contravariant indices of the second.
contractionIndices :: Ord n =>
                      Tensor (Sh n v i) j v e
                   -> Tensor i (Sh n v i) v e
                   -> S.Set n
contractionIndices t1 t2 =
  S.intersection (ixLabels $ coIx t1) (ixLabels $ contraIx t2)


-- | The outer product of two tensors is defined over the non-empty intersection of the contravariant indices of the first with the covariant ones of the second.
outerProdIndices :: Ord n =>
                      Tensor i (Sh n v j) v e
                   -> Tensor (Sh n v i) j v e
                   -> S.Set n
outerProdIndices t1 t2 =
  S.intersection (ixLabels $ contraIx t1) (ixLabels $ coIx t2)



{-
NB: two tensors can be contracted over an index 'i' if this appears 
-}
  


-- contract :: Tensor2 i j v e -> Tensor2 i j v e -> b -- Tensor2 i j v e 
-- contract (T2 ilo1 ihi1 v1) (T2 ilo2 ihi2 v2) = undefined





-- | static-rank Tensor

-- -- | The 'Tensor' type with statically known shape. Tensor data entries are stored as one single array
-- data Tensor i a where
--   Tensor :: Sh i -> V.Vector a -> Tensor (Sh i) a



-- -- | Construct a tensor given a shape and a vector of entries
-- mkT :: Sh i -> V.Vector a -> Tensor (Sh i) a
-- mkT = Tensor

-- instance Functor (Tensor i) where
--   fmap f (Tensor sh v) = Tensor sh (f <$> v)


-- pure' :: a -> Tensor (Sh Z) a
-- pure' = mkT Z . V.singleton

-- instance (Eq a) => Eq (Tensor i a) where
--   (Tensor sh1 d1) == (Tensor sh2 d2) = sh1 == sh2 && d1 == d2

-- instance (Show a) => Show (Tensor i a) where
--   show (Tensor sh d) = unwords [show sh, show $ V.take 5 d, "..."]


 

-- -- | Access the shape of a 'Tensor'
-- tshape :: Tensor sh a -> sh
-- tshape (Tensor sh _) = sh

-- -- | Access the raw data of a 'Tensor'
-- tdata :: Tensor sh a -> V.Vector a
-- tdata (Tensor _ td) = td

-- -- | Number of nonzero tensor elements
-- nnz :: Tensor i a -> Int
-- nnz (Tensor _ td) = V.length td

-- -- | Tensor rank
-- rank :: Tensor i a -> Int
-- rank (Tensor sh _) = Shape.rank sh

-- -- | Tensor dimensions
-- dim :: Tensor i a -> [Int]
-- dim (Tensor sh _) = Shape.dim sh















-- | playground, for future use

-- -- | A generic tensor type, polymorphic in the container type as well
-- data GTensor c i a where
--   GTensor :: Sh i -> c a -> GTensor c (Sh i) a
  
-- mkGT :: Sh i -> c a -> GTensor c (Sh i) a
-- mkGT = GTensor









