module Data.Shape (
  Sh(..), mkSh, mkShD, DimE, shDiff,
  Shape(..), rank, dim, Z, (:#), (:.)
  ) where

import Data.Shape.Dynamic.Named -- (Sh(..), mkSh, DimE, shDiff)
import Data.Shape.Types
