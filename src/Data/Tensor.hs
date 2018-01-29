{-# language GADTs #-}
{-# language DeriveFunctor #-}
{-# language TypeOperators #-}
module Data.Tensor where

import qualified Data.Vector.Unboxed as V

-- import Data.Word (Word32, Word64)


data Z
data sh :. e 

-- data Shape sh where
--   Z :: Shape Z
--   (:.) :: Shape sh -> Int -> Shape (sh :. Int)

-- dim :: Shape sh -> Int
-- dim Z = 0
-- dim (sh :. _) = dim sh + 1

data Shape i where
  Z :: Shape Z
  (:.) :: Shape i -> DMD i -> Shape (i :. DMD i)

dimSh :: Shape sh -> Int
dimSh Z = 0
dimSh (d :. c) = dimSh d + dim c



-- * Dimension metadata

-- | To define a /dense/ dimension we only need the dimensionality parameter
data DMDDense i = DMDDense { dDim :: Int } deriving (Eq, Show)

-- | To define a /sparse/ dimension we need a cumulative array, an index array and a dimensionality parameter
data DMDSparse i = DMDSparse {
      sCml :: Maybe (V.Vector i)
    , sIdx :: V.Vector i
    , sDim :: Int }
  deriving (Eq, Show)

-- | A tensor dimension can be either dense or sparse.
--
-- Example: the CSR format is /dense/ in the first index (rows) and /sparse/ in the second index (columns)
newtype DMD i = DMD (Either (DMDDense i) (DMDSparse i)) deriving (Eq, Show)


-- | The 'Tensor' type. Tensor data entries are stored as one single array
data Tensor i a = Tensor {
    tensorData :: V.Vector a
  , tensorIxs :: [DMD i]
                         } deriving (Eq, Show)

dim :: DMD i -> Int
dim (DMD ed) = either dDim sDim ed

-- -- | Tensor dimension
-- dims :: Tensor i a -> [Int]
-- dims t = dim <$> tensorIxs t


denseDim :: Int -> DMD Int
denseDim = DMD . Left . DMDDense

sparseDim :: Maybe (V.Vector i) -> V.Vector i -> Int -> DMD i
sparseDim pos ix d = DMD $ Right $ DMDSparse pos ix d


-- | Tensor rank (# of dimensions)
rank :: Tensor i a -> Int
rank = length . tensorIxs


-- | Construction of dense vector 
vectorFromListD :: V.Unbox a => [a] -> Tensor Int a
vectorFromListD ll = Tensor (V.fromList ll) [denseDim (length ll)]




{- |
IN: Tensor reduction syntax (Einstein notation)

OUT: stride program (how to read/write memory)
-}





-- * A possible abstract syntax

data Expr a where
  Const :: a -> Expr a 
  -- ^ Sum two expressions
  (:+:) :: Expr a -> Expr a -> Expr a
  -- ^ Reduce over one or more indices
  (:*:) :: Expr a -> Expr a -> Expr a





--



-- data Expr a =
--     Const a
--   | Expr a :+: Expr a
--   | Expr a :*: Expr a
--   deriving (Eq, Show)


-- eval :: Num t => Expr t -> t
-- eval (Const x) = x
-- eval (a :+: b) = eval a + eval b
-- eval (a :*: b) = eval a * eval b


{- | taco compiles a tensor expression (e.g. C = A_{ijk}B_{k} ) into a series of nested loops.

dimensions : can be either dense or sparse

internally, tensor data is stored in /dense/ vectors

"contract A_{ijk}B_{k} over the third index"

-}


-- data Ops a = Reduce Int a a deriving (Eq, Show)







-- reduce imode mata matb 


-- data Expr a =
--     Const a
--   | Expr a :+: Expr a
--   | Expr a :*: Expr a
--   deriving (Eq, Show)


-- eval :: Num t => Expr t -> t
-- eval (Const x) = x
-- eval (a :+: b) = eval a + eval b
-- eval (a :*: b) = eval a * eval b



-- --












