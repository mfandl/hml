module HopfieldMat where

import Numeric.LinearAlgebra

pat :: Vector R
pat = fromList 
  [  1,  1,  1,  1,  1
  , -1, -1,  1, -1, -1
  , -1, -1,  1, -1, -1
  , -1, -1,  1, -1, -1
  , -1, -1,  1, -1, -1]


noisy :: Vector R
noisy =  fromList
  [  1,  1,  1, -1,  1
  , -1, -1,  1, -1, -1
  , -1, -1, -1, -1, -1
  ,  1, -1, -1, -1, -1
  , -1, -1,  1, -1, -1]

initialWeights :: Vector R -> Matrix R
initialWeights input = let ln = size input
                        in (ln><ln) $ repeat 0

train :: Vector R -> Matrix R 
train p = (outer p p) * zeroDiagonal
  where
    zeroDiagonal = (sp><sp)(repeat 1) - ident sp 
    sp = size p

fwd :: Matrix R -> Vector R -> Vector R
fwd w p = cmap signum $ w #> p
