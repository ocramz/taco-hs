{-# language GADTs #-}
{-# language DeriveFunctor #-}
{-# language TypeOperators #-}
module Data.Tensor where

import qualified Data.Vector.Unboxed as V

-- import Data.Word (Word32, Word64)

import Data.Shape (Sh(..), dim, rank,
                   -- * Some synonyms
                   D1, D2, CSR, COO, mkD2, mkCSR, mkCOO)
import Data.Dim

-- | The 'Tensor' type. Tensor data entries are stored as one single array
data Tensor i a where
  T :: Sh i -> V.Vector a -> Tensor (Sh i) a

instance (V.Unbox a, Eq a) => Eq (Tensor i a) where
  (T sh1 d1) == (T sh2 d2) = sh1 == sh2 && d1 == d2
  

mkT :: Sh i -> V.Vector a -> Tensor (Sh i) a
mkT = T




-- | Number of nonzero tensor elements
nnz :: V.Unbox a => Tensor i a -> Int
nnz (T _ td) = V.length td

-- nnz :: V.Unbox a => Tensor t a -> Int


-- mkT :: Sh i -> V.Vector a -> Tensor i a
-- mkT = flip Tensor





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











