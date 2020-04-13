module Perceptron where

import Prelude hiding ((<>))
import Numeric.LinearAlgebra
import qualified Data.Monoid

feed :: Matrix R -> Vector R -> Vector R
feed weights = flatten . fwd weights . asColumn

fwd :: Matrix R -> Matrix R -> Matrix R
fwd weights inputs = let cs = cols inputs
                         bias = (1><cs) $ repeat (-1) :: Matrix R
                      in step $ inputs <> weights

update :: Double -> Matrix R -> Matrix R -> Matrix R -> Matrix R
update learningRate weights targets inputs  =
  weights - scale learningRate (tr withBias <> (activations - targets))
    where withBias = let cs = cols inputs
                         bias = (1><cs) $ repeat (-1) :: Matrix R
                      in inputs === bias
          activations = fwd weights inputs
