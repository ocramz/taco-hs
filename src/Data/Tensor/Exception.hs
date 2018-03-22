{-# language DeriveDataTypeable #-}
module Data.Tensor.Exception where

import Data.Typeable
import Control.Exception


data TException = MismatchedDims Int Int
  | IncompatDataSize Int Int
  deriving (Eq, Typeable)
instance Show TException where
  show e = case e of
    MismatchedDims d1 d2 -> unwords [show d1, "/=", show d2]
    IncompatDataSize ddat d -> unwords [show ddat, ">", show d]
instance Exception TException

