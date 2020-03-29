module Perceptron where

import Numeric.LinearAlgebra ( Matrix
                             , R
                             , (><))

mat :: Matrix R
mat = (4><3)
  [  1,  2,  3
  ,  4,  5,  6
  ,  7,  8,  9
  , 10, 11, 12]

matB :: Matrix R
matB = (4><3)
  [  1,  2,  3
  ,  4,  5,  6
  ,  7,  8,  9
  , 10, 11, 12]


someFunc :: IO ()
someFunc = putStrLn . show $ mat + matB
