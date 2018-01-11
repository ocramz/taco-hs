module Data.Tensor where

import qualified Data.Vector.Unboxed as V

-- import Data.Word (Word32, Word64)




-- * Dimension metadata

-- | To define a /dense/ dimension we only need its scalar size
data DMDDense i = DMDDense { dmdSize :: i } deriving (Eq, Show)

-- | To define a /sparse/ dimension we need both a position and an index array
data DMDSparse i = DMDSparse {
      dmdPos :: V.Vector i
    , dmdIdx :: V.Vector i }
  deriving (Eq, Show)


-- | A tensor dimension can be either dense or sparse.
--
-- Example: the CSR format is /dense/ in the first index (rows) and /sparse/ in the second index (columns)
newtype DMD i = DMD (Either (DMDDense i) (DMDSparse i)) deriving (Eq, Show)

denseDim :: i -> DMD i
denseDim = DMD . Left . DMDDense

sparseDim :: V.Vector i -> V.Vector i -> DMD i
sparseDim pos ix = DMD $ Right $ DMDSparse pos ix

-- | Tensor data entries are always stored as one single array, no matter the order and sparsity
-- lnewtype TensorData a = TD {    -- 
--     _tData :: V.Vector a
--   } deriving (Eq, Show)

data Tensor i a = T {
    _tData :: V.Vector a
  , _tDim :: [DMD i]
  } deriving (Eq, Show)

rank :: Tensor i a -> Int
rank (T _ td) = length td

vectorFromListD :: V.Unbox a => [a] -> Tensor Int a
vectorFromListD ll = T (V.fromList ll) [denseDim (length ll)]


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












