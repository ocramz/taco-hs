{-# language GADTs #-}
{-|
* inner product of two vectors
* matrix-vector action
* matrix-matrix product
-}
module Data.Tensor.Compiler.MultiStage where



data E a where
  Ekonst :: a -> E a



-- | User-facing syntax
data E1 a =
    K1 a
  | Dot a a -- (E1 a) (E1 a)
  deriving (Eq, Show)

-- | Evaluate user-facing syntax into internal one
evalE1 :: E1 a -> E2 a
evalE1 expr = case expr of
  K1 x -> K2 x
  Dot a b -> Fold Add (ZipWith Mul (K2 a) (K2 b))

-- k1 :: a -> E1 a
-- k1 = K1

dot :: a -> a -> E1 a
dot = Dot

-- | Internal representation
data E2 a =
    K2 a
  | ZipWith BinOp (E2 a) (E2 a)
  | Fold BinOp (E2 a) deriving (Eq, Show)

-- | Evaluate the internal representation into a concrete result. NB: the internal and returned type, [b], is constrained by the evaluation functions `zipWith`/`foldr`. In general this would be a tensor data structure rather than a list.
evalE2 :: Fractional b => b -> E2 [b] -> [b]
evalE2 z expr = case expr of
  K2 x -> x
  ZipWith op v1 v2 -> zipWith (evalBinOp op) (evalE2 z v1) (evalE2 z v2)
  Fold op v -> [foldr (evalBinOp op) z (evalE2 z v)]

evalE2' :: E2 [Double] -> [Double]
evalE2' = evalE2 0  

-- | E2 combinators
k2 :: a -> E2 a
k2 = K2
addE2 :: a -> a -> E2 a
addE2 a b = ZipWith Add (k2 a) (k2 b)
mulE2 :: a -> a -> E2 a
mulE2 a b = ZipWith Mul (k2 a) (k2 b)

-- | Chained E1 -> E2 evaluation
evalE :: Fractional b => b -> E1 [b] -> [b]
evalE z = evalE2 z . evalE1

-- data UnOp = Sqrt deriving (Eq, Show)
data BinOp = Add | Sub | Mul | Div deriving (Eq, Show)
evalBinOp :: Fractional a => BinOp -> a -> a -> a
evalBinOp op = case op of
  Add -> (+)
  Sub -> (-)
  Mul -> (*)
  Div -> (/)

-- v1, v2 :: E1 [Int]
-- v1 = k [1..5]
-- v2 = k [3..7]
