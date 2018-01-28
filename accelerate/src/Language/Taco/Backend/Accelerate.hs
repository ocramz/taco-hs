module Language.Taco.Backend.Accelerate where




-- * Trans-compilation from Expr1 AST to Expr2

data Expr1 a =
    K1 a
  | Sum (Expr1 a) (Expr1 a)
  | Inner (Expr1 a) (Expr1 a)
  deriving (Eq, Show)


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
