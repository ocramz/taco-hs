{-# language GADTs #-}
module Data.Tensor.TwoParameter where

import qualified Data.Vector as V

import qualified Data.Shape as Shape (Shape(..), dim, rank)
import Data.Shape (Sh(..), Z, D1, D2)
import qualified Data.Dim as Dim

-- | Tensor with separate co- and contra-variant index parameters

-- | Covariant indices, contravariant indices, container type, element type
data Tensor2 i j v e where
  T2 ::    
       Sh i                    
    -> Sh i                    
    -> v e  
    -> Tensor2 (Sh i) (Sh i) v e


newtype T i e = T { unT :: Tensor2 (Sh i) (Sh i) V.Vector e}

newtype Tl i e = Tl { unTl :: Tensor2 (Sh i) (Sh i) [] e}
