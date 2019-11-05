module Main where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Vector as V

-- import Data.Tensor
import Data.Tensor.Internal (csPtrV)


main :: IO ()
main = hspec $ do
  describe "Data.Tensor.Internal" $ do
    it "ptrV : computes the pointer vector of a sorted vector of integers" $ 
      csPtrV id 3 (V.fromList [0,0,1,2]) `shouldBe` V.fromList [0,2,3,4]



-- -- test data

-- t0 = mkTensorUnsafe tdco tdcontra [1..12]

-- data Ix = I | J | K | L | M | N deriving (Eq, Show, Ord, Enum)

-- tdco, tdcontra :: Sh Ix [] Int
-- tdco = mkSh $ zip [I, J ..] [Left (Dd 3), Left (Dd 2)]

-- tdcontra = mkSh $ zip [K] [Left (Dd 2)]
   
