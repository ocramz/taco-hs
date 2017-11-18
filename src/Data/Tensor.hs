module Data.Tensor where

import qualified Data.Vector.Unboxed as V

import Data.Word (Word32, Word64)




-- * Dimension metadata

-- | To define a dense dimension we only need its scalar size
data DMDDense i = DMDDense { dmdSize :: i } deriving (Eq, Show)

-- | To define a sparse dimension we need both a position and an index array
data DMDSparse i = DMDSparse {
      dmdPos :: V.Vector i
    , dmdIdx :: V.Vector i }
  deriving (Eq, Show)


-- | A tensor dimension can be either dense or sparse
newtype DMD i = DMD (Either (DMDDense i) (DMDSparse i)) deriving (Eq, Show)

newtype TensorData a = TD { _tensorData :: V.Vector a } deriving (Eq, Show)
