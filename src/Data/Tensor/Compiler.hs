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
    mkVar, contract
    -- * Tensor types
  , Tensor(..), Sh(..), Dd(..), Sd(..)
    -- * Syntax
  , Phoas(..)
    -- * Exceptions
  , CException
  )where

import Data.Typeable
import "exceptions" Control.Monad.Catch (MonadThrow(..), throwM, MonadCatch(..), catch)
import Control.Exception (Exception(..))

import Control.Applicative (liftA2, (<|>))

import Data.Tensor (Tensor(..), Sh(..), Dd(..), Sd(..), tshape, tdata, nnz, rank, dim)
import Data.Tensor.Compiler.PHOAS (Phoas(..), let_, let2_, var, lift1, lift2, eval)






-- | Inject a 'Tensor' constant into a 'Var', while ensuring that all the contraction indices are compatible with those of the tensor.
--
-- Throws exceptions if any index is nonnegative or too large for the shape of the given tensor
mkVar :: MonadThrow m => [Int] -> Tensor i a -> m (Phoas (Tensor i a))
mkVar [] t = pure $ var t
mkVar (i:is) t
  | i < 0 =
    throwM $ IncompatIx "Index must be non-negative"
  | i > rank t - 1 =
    throwM $ IncompatShape "Index exceeds is incompatible with the tensor rank"
  | otherwise = mkVar is t


-- | Tensor contraction
contract :: MonadThrow m =>
                  [Int]         -- ^ Contraction indices
                  -> Tensor i a 
                  -> Tensor i a
                  -> (Tensor i a -> Tensor i a -> Phoas (Tensor i a))  -- ^ Contraction function
                  -> m (Phoas (Tensor i a))
contract ixs t1 t2 f = do
  t1p <- mkVar ixs t1
  t2p <- mkVar ixs t2
  pure $ let2_ t1p t2p f


-- | Exceptions
data CException = IncompatShape String | IncompatIx String deriving (Eq, Typeable)
instance Show CException where
  show c = case c of
    IncompatShape str -> unwords ["Incompatible shape:", str]
    IncompatIx str -> unwords ["Incompatible index:", str]
instance Exception CException where






