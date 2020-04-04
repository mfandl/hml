module HopfieldMat ( initialWeights
                   , energy
                   , feed
                   , train
                   ) where

import Numeric.LinearAlgebra

initialWeights :: Int -> Matrix R
initialWeights l = (l><l) $ repeat 0

train :: Vector R -> Matrix R -> Matrix R
train p w = (outer p p) * zeroDiagonal + w
  where
    zeroDiagonal = (sp><sp)(repeat 1) - ident sp 
    sp = size p

feed :: Matrix R -> Vector R -> Vector R
feed w = cmap signum . (w #>)

energy :: Matrix R -> Vector R -> Double
energy w p = -0.5 * (sumElements $ p * (w #> p))
