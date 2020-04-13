module Perceptron where

import Prelude hiding ((<>))
import Numeric.LinearAlgebra
import qualified Data.Monoid
import System.IO.Unsafe

orInputs :: Matrix R
orInputs = (4><2)
  [0, 0,
   0, 1,
   1, 0,
   1, 1]

orTargets :: Matrix R
orTargets = col [0, 1, 1, 1]

orInitialWeights :: Matrix R
orInitialWeights = initialWeights (cols orInputs) 1

-- todo: set the initial weights correctly
initialWeights :: Int -> Int -> Matrix R
initialWeights r c = unsafePerformIO $ rand (r + 1) c

addBiasNodes :: Matrix R -> Matrix R
addBiasNodes m = let rs = rows m
                     bias = (rs><1) $ repeat (-1)
                  in m ||| bias

feed :: Matrix R -> Vector R -> Vector R
feed weights = flatten . fwd weights . asColumn

fwd :: Matrix R -> Matrix R -> Matrix R
fwd weights inputs = let withBias = addBiasNodes inputs
                      in step $ withBias <> weights

update :: Double -> Matrix R -> Matrix R -> Matrix R -> Matrix R
update learningRate weights targets inputs  =
  weights - scale learningRate (tr withBias <> (activations - targets))
    where withBias = addBiasNodes inputs
          activations = fwd weights inputs
