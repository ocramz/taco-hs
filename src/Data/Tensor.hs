{-# language GADTs #-}
{-# language DeriveFunctor #-}
{-# language TypeOperators #-}
module Data.Tensor where

import qualified Data.Vector.Unboxed as V

-- import Data.Word (Word32, Word64)

import Data.Shape
import Data.Dim


-- | The 'Tensor' type. Tensor data entries are stored as one single array
data Tensor i a = Tensor {
    tensorData :: V.Vector a
  , tensorShape :: Sh i
                         } deriving (Eq, Show)

nnz :: V.Unbox a => Tensor t a -> Int
nnz (Tensor td _) = V.length td

-- dim :: DMD i -> Int
-- dim (DMD ed) = either dDim sDim ed

-- -- | Tensor dimension
-- dims :: Tensor i a -> [Int]
-- dims t = dim <$> tensorIxs t


-- denseDim :: Int -> DMD Int
-- denseDim = DMD . Left . DMDDense

-- sparseDim :: Maybe (V.Vector i) -> V.Vector i -> Int -> DMD i
-- sparseDim pos ix d = DMD $ Right $ DMDSparse pos ix d


-- -- | Tensor rank (# of dimensions)
-- rank :: Tensor i a -> Int
-- rank = length . tensorIxs


-- -- | Construction of dense vector 
-- vectorFromListD :: V.Unbox a => [a] -> Tensor Int a
-- vectorFromListD ll = Tensor (V.fromList ll) [denseDim (length ll)]




{- |
IN: Tensor reduction syntax (Einstein notation)

OUT: stride program (how to read/write memory)


taco compiles a tensor expression (e.g. C = A_{ijk}B_{k} ) into a series of nested loops.

dimensions : can be either dense or sparse

internally, tensor data is stored in /dense/ vectors

"contract A_{ijk}B_{k} over the third index"

-}




-- * A possible abstract syntax

data Expr a =
    Const a
  | Contract Int (Expr a) (Expr a)
  -- | Expr a :+: Expr a
  -- | Expr a :*: Expr a
  -- | Expr a :-: Expr a
  -- | Expr a :/: Expr a
  deriving (Eq, Show)

-- -- | trivial recursive evaluation function
-- eval :: Num t => Expr t -> t
-- eval (Const x) = x
-- eval (a :+: b) = eval a + eval b
-- eval (a :*: b) = eval a * eval b




-- | GADT syntax

-- data Expr a where
--   Const :: a -> Expr a 
--   -- ^ Sum (elementwise) two expressions
--   (:+:) :: Expr a -> Expr a -> Expr a
--   -- ^ Multiply (elementwise) two expressions
--   (:*:) :: Expr a -> Expr a -> Expr a
--   -- ^ Subtract (elementwise) two expressions
--   (:-:) :: Expr a -> Expr a -> Expr a











