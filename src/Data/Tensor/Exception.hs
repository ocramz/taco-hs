{-# language DeriveDataTypeable #-}
module Data.Tensor.Exception where

import Data.Typeable
import Control.Exception


-- | Exceptions

data TException = MismatchedDims Int Int
  | IncompatDataSize Int Int
  | IncompatIx String 
  deriving (Eq, Typeable)
instance Show TException where
  show e = case e of
    MismatchedDims d1 d2 -> unwords ["Incompatible dimensions:", show d1, "/=", show d2]
    IncompatDataSize ddat d -> unwords ["Incompatible data size", show ddat, ">", show d]
    IncompatIx str -> unwords ["Incompatible index:", str]
instance Exception TException


-- data CException = IncompatShape String | IncompatIx String deriving (Eq, Typeable)
-- instance Show CException where
--   show c = case c of
--     IncompatShape str -> unwords ["Incompatible shape:", str]
--     IncompatIx str -> unwords ["Incompatible index:", str]
-- instance Exception CException where
