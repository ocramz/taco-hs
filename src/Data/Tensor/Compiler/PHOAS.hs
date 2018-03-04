{-# language GADTs, RankNTypes #-}
module Data.Tensor.Compiler.PHOAS where


-- | Parametric higher-order abstract syntax (PHOAS), after B. Oliveira, A. Loeh, `Abstract Syntax Graphs for Domain Specific Languages`  
data Phoas a where
  Var :: a -> Phoas a
  Let :: Phoas a -> (a -> Phoas b) -> Phoas b
  -- Let :: Phoas a -> (a -> Phoas a) -> Phoas a

-- | Inject a constant into the abstract syntax
var :: a -> Phoas a
var = Var

-- | Bind a variable into a closure
let_ :: Phoas a -> (a -> Phoas b) -> Phoas b
let_ = Let

-- | Bind two variables into a closure
let2_ :: Phoas a -> Phoas b -> (a -> b -> Phoas c) -> Phoas c
let2_ a b f = let_ a $ \xa ->
  let_ b $ \xb -> f xa xb

-- letP_ :: Phoas a -> (Phoas a -> Phoas a) -> Phoas a
-- letP_ e f = let_ e (f . Var)

-- letP2_ :: Phoas a -> Phoas a -> (Phoas a -> Phoas a -> Phoas a) -> Phoas a
-- letP2_ a b f = let2_ a b (\x y -> f (Var x) (Var y))


-- instance Show a => Show (Phoas a) where
--   show e = case e of
--     Var x -> show x
--     -- Let e f -> unwords



-- | Helper functions  


lift1 :: (a -> b) -> a -> Phoas b
lift1 f = Var . f -- Lift1

lift2 :: (a -> b -> c) -> a -> b -> Phoas c
lift2 f a b = Var (f a b) -- Lift2


plus :: Num a => a -> a -> Phoas a
plus = lift2 (+)


-- | Benchmark: `tree 50` should compute the answer instantly.
--
-- This proves that the PHOAS formulation preserves variable sharing
treeE :: Integer -> Phoas Integer
treeE 0 = Var 1
treeE n = let_ (treeE (n - 1)) $ \a -> a `plus` a 




-- | Semantic function for evaluation
eval :: Phoas a -> a
eval expr = case expr of
  Var x -> x
  Let e f -> eval (f (eval e))


-- -- | Semantic function for pretty-printing
-- type ClosedExpr = forall a . Phoas a

-- pprint :: ClosedExpr -> String
-- pprint expr = go expr 0
--   where
--     go :: Phoas String -> Int -> String
--     go (Var x) _ = x
--     go (Let e f) c = unwords ["(let", v, "=", go e (c+1), "in", go (f v) (c+1),")"]
--       where
--         v = "v" ++ show c




-- * A possible abstract syntax


{-|
* inner product of two vectors
* matrix-vector action
* matrix-matrix product
-}

-- | "User-facing" syntax
data E1 a =
    K1 a
  | Dot a a -- (E1 a) (E1 a)
  -- | CW2 BinOp (E1 a) (E1 a)
  deriving (Eq, Show)

evalE1 :: E1 a -> E2 a
evalE1 expr = case expr of
  K1 x -> K2 x
  Dot a b -> Fold Add (ZipWith Mul (K2 a) (K2 b))

k1 :: a -> E1 a
k1 = K1
dot :: a -> a -> E1 a
dot = Dot


-- | Internal representation
data E2 a =
    K2 a
  | ZipWith BinOp (E2 a) (E2 a)
  | Fold BinOp (E2 a) deriving (Eq, Show)

-- | Evaluate the internal representation into a concrete result
evalE2 :: Fractional b => b -> E2 [b] -> [b]
evalE2 z expr = case expr of
  K2 x -> x
  ZipWith op v1 v2 -> zipWith (evalBinOp op) (evalE2 z v1) (evalE2 z v2)
  Fold op v -> [foldr (evalBinOp op) z (evalE2 z v)]

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







-- data Index i where
--   I1 :: i -> Index i
--   I2 :: i -> i -> Index (i, i)

-- -- | Expressions with tensor operands, e.g. "contract A_{ijk}B_{k} over the third index"

-- -- | User-facing grammar:
-- data Expr a where
--   -- | Introduce a constant in the AST
--   Konst :: a -> Expr a
--   -- | Tensor contraction
--   Contr :: i -> (Expr a -> Expr a -> Expr a) -> Expr a
--   -- | Binary componentwise operation
--   CW2 :: (a -> a -> a) -> Expr a -> Expr a -> Expr a

-- k :: a -> Expr a
-- k = Konst

-- (|*|), (|+|) :: Num a => Expr a -> Expr a -> Expr a
-- (|*|) = CW2 (*)  
-- (|+|) = CW2 (+)

-- dot = Contr 1 (|+|)





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
