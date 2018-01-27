{-# language GADTs #-}
{-# language DeriveFunctor #-}
module Data.Tensor where

import qualified Data.Vector.Unboxed as V

-- import Data.Word (Word32, Word64)


-- * Dimension metadata

-- | To define a /dense/ dimension we only need the dimensionality parameter
data DMDDense i = DMDDense { dDim :: Int } deriving (Eq, Show)

-- | To define a /sparse/ dimension we need a cumulative array, an index array and a dimensionality parameter
data DMDSparse i = DMDSparse {
      sCml :: V.Vector i
    , sIdx :: V.Vector i
    , sDim :: Int }
  deriving (Eq, Show)

-- | A tensor dimension can be either dense or sparse.
--
-- Example: the CSR format is /dense/ in the first index (rows) and /sparse/ in the second index (columns)
newtype DMD i = DMD (Either (DMDDense i) (DMDSparse i)) deriving (Eq, Show)


-- | Tensor data entries are stored as one single array
data Tensor i a = Tensor {
    tensorData :: V.Vector a
  , tensorIxs :: [DMD i]
                         } deriving (Eq, Show)

dim :: DMD i -> Int
dim (DMD ed) = either dDim sDim ed

dims :: Tensor i a -> [Int]
dims t = dim <$> tensorIxs t


denseDim :: Int -> DMD Int
denseDim = DMD . Left . DMDDense

sparseDim :: V.Vector i -> V.Vector i -> Int -> DMD i
sparseDim pos ix d = DMD $ Right $ DMDSparse pos ix d



rank :: Tensor i a -> Int
rank = length . tensorIxs


vectorFromListD :: V.Unbox a => [a] -> Tensor Int a
vectorFromListD ll = Tensor (V.fromList ll) [denseDim (length ll)]




data Expr a where
  Const :: a -> Expr a 
  -- ^ Sum two expressions
  (:+:) :: Expr a -> Expr a -> Expr a
  -- ^ Reduce over one or more indices
  (:*:) :: Expr a -> Expr a -> Expr a








data Expr1 a =
    K1 a
  | Sum (Expr1 a) (Expr1 a)
  | Inner (Expr1 a) (Expr1 a)
  deriving (Eq, Show)

-- eval e = case e of
--   -- K1 x -> K2 x
--   Sum x y -> ZipWith "+" (K2 x) (K2 y)

eval1 :: Expr1 a -> Expr2 a
eval1 e = case e of
  K1 x -> K2 x
  Sum x y -> ZipWith "+" (eval1 x) (eval1 y)
  Inner x y -> Fold "+" "z" $ ZipWith "*" (eval1 x) (eval1 y)

k :: a -> Expr1 a
k = K1

(.+.) :: Expr1 a -> Expr1 a -> Expr1 a
(.+.) = Sum

inner :: Expr1 a -> Expr1 a -> Expr1 a
inner = Inner

-- type Op1 = String
type Op2 = String
type Z = String

data Expr2 a =
    K2 a
  | ZipWith Op2 (Expr2 a) (Expr2 a)
  | Fold Op2 Z (Expr2 a)

instance Show a => Show (Expr2 a) where
  show (K2 x) = show x
  show (ZipWith fs x y) = unwords ["(zipWith", fs, show x, show y, ")"]
  show (Fold o2 z x) = unwords ["(fold", o2, z, show x, ")"]






--









-- data Expr a =
--     Const a
--   | Expr a :+: Expr a
--   | Expr a :*: Expr a
--   deriving (Eq, Show)


-- eval :: Num t => Expr t -> t
-- eval (Const x) = x
-- eval (a :+: b) = eval a + eval b
-- eval (a :*: b) = eval a * eval b


{- | taco compiles a tensor expression (e.g. C = A_{ijk}B_{k} ) into a series of nested loops.

dimensions : can be either dense or sparse

internally, tensor data is stored in /dense/ vectors

"contract A_{ijk}B_{k} over the third index"

-}


-- data Ops a = Reduce Int a a deriving (Eq, Show)







-- reduce imode mata matb 


-- data Expr a =
--     Const a
--   | Expr a :+: Expr a
--   | Expr a :*: Expr a
--   deriving (Eq, Show)


-- eval :: Num t => Expr t -> t
-- eval (Const x) = x
-- eval (a :+: b) = eval a + eval b
-- eval (a :*: b) = eval a * eval b



-- --












