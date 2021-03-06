{-# language DeriveFunctor #-}
{-# language GADTs #-}
module Data.Tensor.Compiler.Free where

-- | A free monad given an applicative
data Free f a =
    Pure a
  | Free (f (Free f a)) deriving Functor

instance Applicative f => Applicative (Free f) where
  pure = Pure
  {-# INLINE pure #-}
  Pure a <*> Pure b = Pure $ a b
  Pure a <*> Free mb = Free $ fmap a <$> mb
  Free ma <*> Pure b = Free $ fmap ($ b) <$> ma
  Free ma <*> Free mb = Free $ fmap (<*>) ma <*> mb

instance Applicative f => Monad (Free f) where
  return = pure
  {-# INLINE return #-}
  Pure a >>= f = f a
  Free m >>= f = Free ((>>= f) <$> m)


liftF :: Functor f => f a -> Free f a
liftF k = Free $ Pure <$> k







    
data Lang a next =
    Add a a (a -> next)
  | Konst a (a -> next)
  deriving (Functor)

instance Applicative (Lang a) where
  


(.+.) :: a -> a -> Free (Lang a) a
x .+. y = liftF (Add x y id)

konst :: a -> Free (Lang a) a
konst x = liftF (Konst x id)


-- * Interpretation functions

-- | Evaluator
calc :: Num a => Free (Lang a) a -> a
calc fx = case fx of
  Pure r -> r
  Free (Add x y f) ->
    let z = x + y
    in calc (f z)
  Free (Konst x f) -> calc (f x)


-- | Pretty printer
pprint :: (Num a, Show a) => Free (Lang a) a -> String
pprint fx = case fx of
  Pure _ -> "Done"
  Free (Add x y f) ->
    let z = x + y
        sh = unwords ["x + y =", show z, "\n"]
    in sh ++ pprint (f z)
  Free (Konst x f) -> pprint (f x)


-- | Example program
ex0 :: Num a => Free (Lang a) a
ex0 = do
  a <- konst 1
  b <- konst 2
  a .+. b

-- | Program 'ex0' interpreted with two different interpreters

calcEx0 :: Int
calcEx0 = calc ex0

pprintEx0 :: IO ()
pprintEx0 = putStrLn $ pprint ex0






-- | Playground



-- data Lang0 a z =
--     Fun1 a (a -> z)
--   | Fun2 a a (a -> a -> z)
--   deriving Functor

-- fun1 :: (a -> b) -> a -> Free (Lang0 a) b
-- fun1 f x = liftF (Fun1 x f)

-- fun2 :: (a -> a -> b) -> a -> a -> Free (Lang0 a) b
-- fun2 f x y = liftF (Fun2 x y f)


-- 


-- data Interaction next =
--     Look Direction (Image -> next)
--   | Fire Direction next
--   | ReadLine (String -> next)
--   | WriteLine String (Bool -> next)

-- look :: Direction -> Program Image
-- look dir = liftF (Look dir id)

-- fire :: Direction -> Program ()
-- fire dir = liftF (Fire dir ())

-- readLine :: Program String
-- readLine = liftF (ReadLine id)

-- writeLine :: String -> Program Bool
-- writeLine s = liftF (WriteLine s id)


-- data Toy b f =
--     Output b (b -> f)
--   | Bell f
--   | Done
--   deriving Functor

-- output :: (b -> a) -> b -> Free (Toy b) a
-- output f x = liftF (Output x f)

-- bell :: a -> Free (Toy a) a
-- bell x = liftF (Bell x)

-- done :: Free (Toy b) a
-- done = liftF Done


