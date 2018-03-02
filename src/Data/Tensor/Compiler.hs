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
    contract
    -- * Tensor types
  , Tensor(..), Sh(..), Dd(..), Sd(..)
    -- * Syntax
  , Phoas, eval, var, let_, let2_
    -- * Exceptions
  , CException
  )where

import Data.Typeable
import "exceptions" Control.Monad.Catch (MonadThrow(..), throwM, MonadCatch(..), catch)
import Control.Exception (Exception(..))

import Control.Applicative (liftA2, (<|>))

import Data.Tensor (Tensor(..), Sh(..), Dd(..), Sd(..), tshape, tdata, nnz, rank, dim)
import Data.Tensor.Compiler.PHOAS (Phoas(..), let_, let2_, var, lift1, lift2, eval)







mkVar :: MonadThrow m => [Int] -> Tensor i a -> m (Phoas (Tensor i a))
mkVar ixs0 t = do
  ixs <- mkIxs ixs0 (rank t)
  return $ var t


mkIxs :: MonadThrow m => [Int] -> Int -> m [Int]
mkIxs ixs mm = go ixs []
  where
    go [] acc = pure acc
    go (i:is) acc | i < 0 =
                    throwM $ IncompatIx "Index must be non-negative"
                  | i > mm - 1 =
                    throwM $ IncompatIx $ unwords ["Index must be smaller than", show mm]
                  | otherwise = go is (i : acc)

-- | Tensor contraction
--
-- Inject two 'Tensor' constant into 'Var's, while ensuring that all the contraction indices are compatible with those of the tensors.
--
-- Throws exceptions if any index is nonnegative or too large for the shape of the given tensor.
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






