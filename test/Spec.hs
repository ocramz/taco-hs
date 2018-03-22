module Main where

import Data.Tensor


main :: IO ()
main = putStrLn "Test suite not yet implemented"



-- test data

t0 = mkTensorUnsafe tdco tdcontra [1..12]

data Ix = I | J | K | L | M | N deriving (Eq, Show, Ord, Enum)

tdco, tdcontra :: Sh Ix [] Int
tdco = mkSh $ zip [I, J ..] [Left (Dd 3), Left (Dd 2)]

tdcontra = mkSh $ zip [K] [Left (Dd 2)]
