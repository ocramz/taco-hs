{-# language GADTs #-}
module Data.Tensor.Compiler where

import Control.Monad.Catch
-- import Control.Exception (Exception(..))

import Data.Tensor
import Data.Tensor.Compiler.PHOAS -- (Phoas(..), let_, let2_, lift1, lift2)



-- | throw exceptions related to incompatible data shape

-- contract2 (T sh1 d1) (T sh2 d2) = undefined







