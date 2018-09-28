module Data.Tensor.Compiler.LC where

import Prelude hiding (lookup)
import qualified Data.Map as M

{-

https://github.com/sdiehl/write-you-a-haskell/blob/master/chapter4/untyped/Eval.hs

-}

type Name = String

data Expr e =
    Lit e
  | Var Name
  | Lam Name (Expr e)
  | App (Expr e) (Expr e) deriving (Eq, Show)

data Value a =
    Const a 
  | Clos Name (Expr a) (Scope a)  -- ^ Closure of an expression over a scope

newtype Scope a = Scope (M.Map Name (Value a))

instance Show a => Show (Value a) where
  show (Const x) = show x
  show _ = "<<closure>>"

eval :: Scope a -> Expr a -> Maybe (Value a)
eval env ex = case ex of
  Lit e -> Just (Const e)
  Var vn -> lookup env vn
  Lam vn e -> Just $ Clos vn e env
  App a b -> do
    x1 <- eval env a
    x2 <- eval env b
    apply x1 x2

apply :: Value a -> Value a -> Maybe (Value a)
apply v ex2 = case v of
  (Clos vn ex env) -> eval (extend env vn ex2) ex
  _ -> Nothing
  

lookup :: Scope a -> Name -> Maybe (Value a)
lookup (Scope env) vn = M.lookup vn env

extend :: Scope a -> Name -> Value a -> Scope a
extend (Scope env) vn x = Scope $ M.insert vn x env
