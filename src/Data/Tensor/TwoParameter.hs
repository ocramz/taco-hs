{-# language GADTs #-}
module Data.Tensor.TwoParameter where

import qualified Data.Vector as V

import qualified Data.Shape as Shape (dim, rank)
-- import Data.Shape.Types
import Data.Shape.Named (Sh(..))
import qualified Data.Dim as Dim

-- | Tensor with separate co- and contra-variant index parameters

-- | Covariant indices, contravariant indices, container type, element type
data Tensor2 i j v e where
  T2 ::    
       Sh n i                    
    -> Sh n i                    
    -> v e  
    -> Tensor2 (Sh n i) (Sh n i) v e

coIx :: Tensor2 i j v e -> i
coIx (T2 ix _ _) = ix

contraIx :: Tensor2 i j v e -> j
contraIx (T2 _ ix _) = ix

-- -- | Can the two tensor operands be contracted?
-- contractible :: (Eq i, Eq j) => Tensor2 i j v e1 -> Tensor2 j i v e2 -> Bool
-- contractible t1 t2 =
--   coIx t1 == contraIx t2 || contraIx t1 == coIx t2


{-
NB: two tensors can be contracted over an index 'i' if this appears 
-}
  


-- contract :: Tensor2 i j v e -> Tensor2 i j v e -> b -- Tensor2 i j v e 
-- contract (T2 ilo1 ihi1 v1) (T2 ilo2 ihi2 v2) = undefined


{- | examples

newtype T i e = T { unT :: Tensor2 (Sh i) (Sh i) V.Vector e}

newtype Tl i e = Tl { unTl :: Tensor2 (Sh i) (Sh i) [] e}
-}
