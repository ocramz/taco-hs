{-# language GADTs #-}
-- {-# language DeriveFunctor #-}
{-# language TypeOperators #-}
{-# language PackageImports #-}
{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}
-- {-# language RankNTypes #-}
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
import Control.Arrow ((&&&))
import qualified Data.Set as S
import qualified Data.Map as M
import "exceptions" Control.Monad.Catch (MonadThrow(..))
import Data.Shape.Types (Shape(..), rank, dim, Z, (:#), (:.))
import Data.Shape.Dynamic.Named -- (Sh(..), DimE, shDiff)
import Data.Dim.Generic (Dd(..), Sd(..))


-- | Covariant indices, contravariant indices, container type, element type
data Tensor i j v e where
  Tensor ::    
       Sh n v i                    
    -> Sh n v i                    
    -> v e  
    -> Tensor (Sh n v i) (Sh n v i) v e

instance (Show i, Show j) => Show (Tensor i j v e) where
  show (Tensor shco shcontra _) =
    unwords ["covariant:", show shco,
             "contravariant:", show shcontra]

nnz :: Foldable v => Tensor i j v e -> Int
nnz (Tensor _ _ v) = length v

tdim :: (Shape i, Shape j) => Tensor i j v e -> ([Int], [Int])
tdim = dim . coIx &&& dim . contraIx where


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
contractionIndices
  :: (Ord k, MonadThrow m, Integral i1) =>
     (DimE v1 i1 -> DimE v1 i1 -> b)
     -> Tensor (Sh k v1 i1) j v2 e1
     -> Tensor i2 (Sh k v1 i1) v3 e2
     -> m (M.Map k b)
contractionIndices f t1 t2 = shDiff f (coIx t1) (contraIx t2)

-- | The outer product of two tensors is defined over the non-empty intersection of the contravariant indices of the first with the covariant ones of the second.
outerProdIndices
  :: (Ord k, MonadThrow m, Integral i1) =>
     (DimE v1 i1 -> DimE v1 i1 -> b)
     -> Tensor i2 (Sh k v1 i1) v2 e1
     -> Tensor (Sh k v1 i1) j v3 e2
     -> m (M.Map k b)
outerProdIndices f t1 t2 = shDiff f (contraIx t1) (coIx t2)





-- test data

t0 = mkTensor tdco tdcontra [1..12]

data Ix = I | J | K | L | M | N deriving (Eq, Show, Ord, Enum)

tdco, tdcontra :: Sh Ix [] Int
tdco = mkSh $ zip [I, J ..] [Left (Dd 3), Left (Dd 2)]

tdcontra = mkSh $ zip [J] [Left (Dd 2)]



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









