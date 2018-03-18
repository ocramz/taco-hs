{-# language DeriveFunctor #-}
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
  | Konst (a -> next)
  -- | Done (a -> next)
  deriving (Functor)

instance Applicative (Lang a) where
--   pure = Done

add :: a -> a -> Free (Lang a) a
add x y = liftF (Add x y id)

konst :: Free (Lang a) a
konst = liftF (Konst id)

-- done :: Free (Lang a) a
-- done = liftF (Done id)

-- -- konst :: Free (Lang a) a
-- -- konst = liftF (Konst id)



-- calc :: Num a => Free (Lang a) a -> IO a
calc fx = case fx of
  Pure r -> return r
  Free (Add x y f) ->
    let z = x + y
    in calc (f z)
  Free (Konst f) -> calc (f 1)


calcExample :: IO Int
calcExample = calc $ do
  a <- konst
  b <- konst  
  add a b



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


