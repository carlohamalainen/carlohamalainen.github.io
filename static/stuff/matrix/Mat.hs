module Mat where

import Control.Monad
import Data.Complex
import Data.Packed.Matrix
import Data.List
import Numeric.Container
import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.HMatrix
import Text.Printf

blah :: Matrix Double
blah = buildMatrix 1 1 $ \(i,j) -> fromIntegral i + fromIntegral j
