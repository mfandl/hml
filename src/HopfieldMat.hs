module HopfieldMat where

import Numeric.LinearAlgebra

pat :: Vector R
pat = fromList 
  [  1,  1,  1,  1,  1
  , -1, -1,  1, -1, -1
  , -1, -1,  1, -1, -1
  , -1, -1,  1, -1, -1
  , -1, -1,  1, -1, -1]

pat2 :: Vector R
pat2 = fromList
  [  1, -1, -1, -1,  1
  ,  1, -1, -1, -1,  1
  ,  1,  1,  1,  1,  1
  ,  1, -1, -1, -1,  1
  ,  1, -1, -1, -1,  1]

noisy :: Vector R
noisy =  fromList
  [  1,  1,  1, -1,  1
  , -1, -1,  1, -1, -1
  , -1, -1, -1, -1, -1
  ,  1, -1, -1, -1, -1
  , -1, -1,  1, -1, -1]

noisy2 :: Vector R
noisy2 = fromList
  [  1, -1, -1, -1,  1
  ,  1, -1,  1, -1,  1
  , -1,  1,  1,  1,  1
  ,  1, -1, -1,  1,  1
  ,  1, -1, -1, -1,  1]

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
