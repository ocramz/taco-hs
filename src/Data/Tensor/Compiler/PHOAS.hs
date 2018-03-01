{-# language GADTs #-}
module Data.Tensor.Compiler.PHOAS where


-- * PHOAS 

data Phoas a where
  Var :: a -> Phoas a
  Let :: Phoas a -> (a -> Phoas a) -> Phoas a
  Let2 :: Phoas a -> Phoas a -> (a -> a -> Phoas a) -> Phoas a

eval :: Phoas t -> t
eval expr = case expr of
  Var x -> x
  Let e f -> eval (f (eval e))
  Let2 e0 e1 f -> eval (f e0' e1') where {e0' = eval e0; e1' = eval e1}

-- | Helper functions  

let_ :: Phoas a -> (Phoas a -> Phoas a) -> Phoas a
let_ e f = Let e (f . Var)

let2_ :: Phoas a -> Phoas a -> (Phoas a -> Phoas a -> Phoas a) -> Phoas a
let2_ e0 e1 f = Let2 e0 e1 (\x y -> f (Var x) (Var y))

lift1 :: (a -> b) -> a -> Phoas b
lift1 f = Var . f -- Lift1

lift2 :: (a -> b -> c) -> a -> b -> Phoas c
lift2 f a b = Var (f a b) -- Lift2

plus :: Num a => Phoas a -> Phoas a -> Phoas a
plus a b = Let2 a b (lift2 (+))



-- | Benchmark: `tree 50` should compute the answer instantly
treeE :: Integer -> Phoas Integer
treeE 0 = Var 1
treeE n = let_ (treeE (n - 1)) (\s -> s `plus` s)




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