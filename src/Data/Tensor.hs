{-# language GADTs #-}
{-# language DeriveFunctor #-}
{-# language TypeOperators #-}
{-# language PackageImports #-}
module Data.Tensor (
  -- * Tensor type
  Tensor(..),
  shape, nnz, rank, dim, 
  -- * Shape type
  Sh(..),
  -- * Dimension types
  Dim.Dd(..), Dim.Sd(..)) where

import qualified Data.Vector as V
-- import qualified Data.Vector.Unboxed as VU

-- import Data.Word (Word32, Word64)
-- import Data.Int (Int32)

import Control.Exception (Exception(..))
import "exceptions" Control.Monad.Catch (MonadThrow(..), throwM, MonadCatch(..), catch)

import qualified Data.Shape as Shape (dim, rank)
import Data.Shape (Sh(..), 
                   Z,
                   D1, D2, CSR, COO, mkD2, mkCSR, mkCOO) 
import qualified Data.Dim as Dim



{- |
IN: Tensor reduction syntax (Einstein notation)

OUT: stride program (how to read/write memory)


taco compiles a tensor expression (e.g. C = A_{ijk}B_{k} ) into a series of nested loops.

dimensions : can be either dense or sparse

internally, tensor data is stored in /dense/ vectors

"contract A_{ijk}B_{k} over the third index"

-}



-- | A generic tensor type, polymorphic in the container type as well
data GTensor c i a where
  GTensor :: Sh i -> c a -> GTensor c (Sh i) a
  
mkGT :: Sh i -> c a -> GTensor c (Sh i) a
mkGT = GTensor


-- | The 'Tensor' type. Tensor data entries are stored as one single array
data Tensor i a where
  T :: Sh i -> V.Vector a -> Tensor (Sh i) a 

-- | Construct a tensor given a shape and a vector of entries
mkT :: Sh i -> V.Vector a -> Tensor (Sh i) a
mkT = T

instance Functor (Tensor i) where
  fmap f (T sh v) = T sh (f <$> v)

-- liftA2' :: (a -> a -> b) -> Tensor i a -> Tensor i a -> Tensor i a 
-- liftA2' f (T sh1 v1) (T sh2 v2) = mkT sh1 (V.zipWith f v1 v2)

pure' :: a -> Tensor (Sh Z) a
pure' = mkT Z . V.singleton

instance (Eq a) => Eq (Tensor i a) where
  (T sh1 d1) == (T sh2 d2) = sh1 == sh2 && d1 == d2

instance (Show a) => Show (Tensor i a) where
  show (T sh d) = unwords [show sh, show $ V.take 5 d, "..."]


 

-- | Access the shape of a tensor
shape :: Tensor sh a -> sh
shape (T sh _) = sh

-- | Number of nonzero tensor elements
nnz :: Tensor i a -> Int
nnz (T _ td) = V.length td

-- | Tensor rank
rank :: Tensor i a -> Int
rank (T sh _) = Shape.rank sh

-- | Tensor dimensions
dim :: Tensor i a -> [Int]
dim (T sh _) = fromIntegral <$> Shape.dim sh





-- * A possible abstract syntax

-- data Index i where
--   I1 :: i -> Index i
--   I2 :: i -> i -> Index (i, i)

-- | Expressions with tensor operands, e.g. "contract A_{ijk}B_{k} over the third index"

-- | User-facing grammar:
-- data Expr a where
--   Const :: a -> Expr a
--   Contract :: i -> Expr a -> Expr a -> Expr a
-- --   -- (:*:) :: Expr a -> Expr a -> Expr a
-- --   -- (:+:) :: Expr a -> Expr a -> Expr a




-- * PHOAS 1

data Phoas a where
  Const :: a -> Phoas a
  -- Var :: Int -> Phoas Int
  Let :: Phoas a -> (a -> Phoas a) -> Phoas a
  Let2 :: Phoas a -> Phoas a -> (a -> a -> Phoas a) -> Phoas a
  -- Let :: (Phoas a -> Phoas b) -> Phoas (a -> b)
  -- App :: Phoas (a -> b) -> Phoas a -> Phoas b

eval expr = case expr of
  -- Var i -> i
  Const x -> x
  Let e f -> eval (f (eval e))
  Let2 e0 e1 f -> eval (f e0' e1') where {e0' = eval e0; e1' = eval e1}

lift1 :: (a -> b) -> a -> Phoas b
lift1 f = Const . f -- Lift1

lift2 :: (a -> b -> c) -> a -> b -> Phoas c
lift2 f a b = Const (f a b) -- Lift2

plus :: Num a => Phoas a -> Phoas a -> Phoas a
plus a b = Let2 a b (lift2 (+))


-- -- * PHOAS 2

-- data Phoas a where
--   Const :: a -> Phoas a
--   Lift1 :: (a -> b) -> Phoas (a -> b)
--   Let :: Phoas a -> (Phoas a -> Phoas b) -> Phoas b
--   Lambda :: (Phoas a -> Phoas b) -> Phoas (a -> b)
--   App1 :: Phoas (a -> b) -> (Phoas a -> Phoas b)

-- eval expr = case expr of
--   -- Const x -> x
--   Let e f -> f e

-- -- lift1 :: (a -> b) -> a -> Phoas b
-- -- lift1 f = Const . f

-- -- lift2 :: (t2 -> t1 -> t) -> Phoas (t2 -> t1 -> t)
-- -- lift2 f = Const $ \a b -> f a b 


-- -- 


-- data Phoas a =
--     Lit Int
--   -- | Lift1 (a -> a) (a -> Phoas a)
--   -- | Add (Phoas a) (Phoas a)
--   | Let (Phoas a) (a -> Phoas a)
--   | Let2 (Phoas a) (Phoas a) (a -> a -> Phoas a)
--   | Var a

-- evalPhoas expr = case expr of
--   Var x -> x
--   Lit i -> i
--   -- Add e0 e1 -> evalPhoas e0 + evalPhoas e1
--   Let e f -> evalPhoas $ f e' where e' = evalPhoas e
--   Let2 e0 e1 f -> evalPhoas $ f e0' e1' where
--     e0' = evalPhoas e0
--     e1' = evalPhoas e1


-- contract ixs (T sh)

-- eval (Const x) = x
-- eval (Contract ixs a b) = undefined



-- data Expr a =
--     Const a
--   | Contract Int (Expr a) (Expr a)
--   -- | Expr a :+: Expr a
--   -- | Expr a :*: Expr a
--   -- | Expr a :-: Expr a
--   -- | Expr a :/: Expr a
--   deriving (Eq, Show)

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











