module Language.Taco.Backend.Vector where

-- import Data.Vector.Unboxed as V

import qualified Data.Vector as V 
import qualified Data.Vector.Mutable as VM

import Control.Monad.ST





-- | consume a list rather than a vector (fewer large allocations wrt Vector)
csPtrV' :: Int -> [Int] -> V.Vector Int
csPtrV' n xs = V.create createf where
  createf :: ST s (VM.MVector s Int)
  createf = do
    let c = 0
    vm <- VM.new (n + 1)
    VM.write vm 0 0  -- write `0` at position 0
    let loop v ll i count | i == n = return ()
                          | otherwise = do
                                let lp = length $ takeWhile (== i) ll
                                    count' = count + lp
                                VM.write v (i + 1) count'
                                loop v (drop lp ll) (succ i) count'
    loop vm xs 0 c
    return vm
