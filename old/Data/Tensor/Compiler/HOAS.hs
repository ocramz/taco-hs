module Data.Tensor.Compiler.HOAS where


data Expr a = Const a | Let (Expr a) (a -> Expr a)

let_ :: Expr a -> (a -> Expr a) -> Expr a
let_ = Let

pprint expr = go expr 0
  where
    go (Const x) _ = show x
    go (Let e f) c = unwords ["(let", v, "=", go e (c+1), "in", go (f c) (c+1), ")"]
      where
        v = "v" ++ show c

eval e = case e of
  Const x -> x
  Let e f -> eval $ f (eval e)

let2_ :: Expr t -> Expr t -> (t -> t -> Expr t) -> Expr t
let2_ a b f = let_ a $ \ax ->
  let_ b $ \bx -> f ax bx

lift2 :: (t1 -> t -> a) -> t1 -> t -> Expr a
lift2 f a b = Const (f a b)

plus :: Num a => Expr a -> Expr a -> Expr a
plus a b = let2_ a b $ lift2 (+)


treeE 0 = Const 1
treeE n = let_ (treeE (n-1)) $ \x -> lift2 (+) x x

-- qf = undefined
