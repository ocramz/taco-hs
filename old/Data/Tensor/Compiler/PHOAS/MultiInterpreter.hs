{-# language GADTs #-}
module Data.Tensor.Compiler.PHOAS.MultiInterpreter where

import Data.Functor.Const
import Data.Functor.Identity



-- | PHOAS-with-multiple-interpretations

data P v a where
  Var2 :: v a -> P v a
  I :: Int -> P v Int
  Plus :: P v Int -> P v Int -> P v Int
  Let2 :: P v a -> (v a -> P v b) -> P v b

eval2 :: P Identity a -> a
eval2 expr = case expr of
  Var2 x -> runIdentity x
  Let2 e f -> eval2 (f $ Identity (eval2 e))
  I n -> n
  Plus a b -> eval2 a + eval2 b  
  


pprint2 :: P (Const String) a -> String
pprint2 = fst . go (("x" ++) . show <$> [1..])
  where
    go :: [String] -> P (Const String) a -> (String, [String])
    go names (I n) = (show n, names)
    go names (Var2 a) = (getConst a, names)
    go (name:names) (Let2 e f) =
      let
        (eStr, names') = go names e
        (fStr, names'') = go names' (f . Const $ name)
      in
        ("let " ++ name ++ " = {" ++ eStr ++ "} in " ++ fStr, names'')
    go names (Plus a b) =
      let
        (aStr, names') = go names a
        (bStr, names'') = go names' b
      in
        (aStr ++ " + " ++ bStr, names'')


treeE2 :: Integer -> P v Int
treeE2 0 = I 1
treeE2 n = Let2 (treeE2 (n - 1)) $ \a -> Var2 a `Plus` Var2 a
