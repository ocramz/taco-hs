{-# language GADTs, RankNTypes #-}
module Data.Tensor.Compiler.PHOAS where


-- | Parametric higher-order abstract syntax (PHOAS), after B. Oliveira, A. Loeh, `Abstract Syntax Graphs for Domain Specific Languages`  
data Phoas a where
  Var :: a -> Phoas a
  Let :: Phoas a -> (a -> Phoas a) -> Phoas a

-- | Inject a constant into the abstract syntax
var :: a -> Phoas a
var = Var

-- | Bind a variable into a closure
let_ :: Phoas a -> (a -> Phoas a) -> Phoas a
let_ = Let

-- | Bind two variables into a closure
let2_ :: Phoas a -> Phoas a -> (a -> a -> Phoas a) -> Phoas a
let2_ a b f = let_ a $ \xa ->
  let_ b $ \xb -> f xa xb

letP_ :: Phoas a -> (Phoas a -> Phoas a) -> Phoas a
letP_ e f = let_ e (f . Var)

letP2_ :: Phoas a -> Phoas a -> (Phoas a -> Phoas a -> Phoas a) -> Phoas a
letP2_ a b f = let2_ a b (\x y -> f (Var x) (Var y))


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
eval :: Phoas t -> t
eval expr = case expr of
  Var x -> x
  Let e f -> eval (f (eval e))


-- | Semantic function for pretty-printing
type ClosedExpr = forall a . Phoas a

pprint :: ClosedExpr -> String
pprint expr = go expr 0
  where
    go :: Phoas String -> Int -> String
    go (Var x) _ = x
    go (Let e f) c = unwords ["(let", v, "=", go e (c+1), "in", go (f v) (c+1),")"]
      where
        v = "v" ++ show c




-- * A possible abstract syntax


{-|
* inner product of two vectors
* matrix-vector action
* matrix-matrix product
-}

data Expr a =
    Konst a
  | Dot (Expr a) (Expr a)
  deriving (Eq, Show)

k :: a -> Expr a
k = Konst




-- data UnOp = Sqrt deriving (Eq, Show)
data BinOp = Add | Sub | Mul | Div deriving (Eq, Show)
evalBinOp :: Fractional a => BinOp -> a -> a -> a
evalBinOp op = case op of
  Add -> (+)
  Sub -> (-)
  Mul -> (*)
  Div -> (/)

data EI a =
    -- CW1 UnOp (Expr a)
  CW2 BinOp (Expr a) (Expr a) deriving (Eq, Show)

v1, v2 :: Expr [Int]
v1 = k [1..5]
v2 = k [3..7]

-- evalEI expr = case expr of
--   CW2 op x y ->






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
