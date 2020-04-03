module HopfieldMat where

import Numeric.LinearAlgebra

initialWeights :: Vector R -> Matrix R
initialWeights input = let ln = size input
                        in (ln><ln) $ repeat 0

train :: Vector R -> Matrix R -> Matrix R
train p w = (outer p p) * zeroDiagonal + w
  where
    zeroDiagonal = (sp><sp)(repeat 1) - ident sp 
    sp = size p

fwd :: Matrix R -> Vector R -> Vector R
fwd w = cmap signum . (w #>)
