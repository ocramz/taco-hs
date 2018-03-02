{-# language GADTs #-}
{-# language PackageImports #-}
{- |
IN: Tensor reduction syntax (Einstein notation)

OUT: stride program (how to read/write memory)


taco compiles a tensor expression (e.g. C = A_{ijk}B_{k} ) into a series of nested loops.

dimensions : can be either dense or sparse

internally, tensor data is stored in /dense/ vectors

"contract A_{ijk}B_{k} over the third index"

-}
module Data.Tensor.Compiler (
    mkVar
    -- * Tensor types
  , Tensor(..), Sh(..)
    -- * Syntax
  , Phoas(..),  )where

import Data.Typeable
import "exceptions" Control.Monad.Catch (MonadThrow(..), throwM, MonadCatch(..), catch)
import Control.Exception (Exception(..))

import Control.Applicative (liftA2, (<|>))

import Data.Tensor (Tensor(..), Sh(..), tshape, tdata, nnz, rank, dim)
import Data.Tensor.Compiler.PHOAS (Phoas(..), let_, let2_, var, lift1, lift2)



-- | throw exceptions related to incompatible data shape

-- contract2 (T sh1 d1) (T sh2 d2) = undefined


mkVar :: MonadThrow m => [Int] -> Tensor i a -> m (Phoas (Tensor i a))
mkVar [] t = pure $ var t
mkVar (i:is) t
  | i < 0 =
    throwM $ IncompatIx "Index must be non-negative"
  | i > rank t - 1 =
    throwM $ IncompatShape "Index exceeds is incompatible with the tensor rank"
  | otherwise = mkVar is t


-- | Exceptions
data CException = IncompatShape String | IncompatIx String deriving (Eq, Typeable)
instance Show CException where
  show c = case c of
    IncompatShape str -> unwords ["Incompatible shape:", str]
    IncompatIx str -> unwords ["Incompatible index:", str]
instance Exception CException where






