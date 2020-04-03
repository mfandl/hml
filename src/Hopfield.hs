module Hopfield where

initialWeights :: [Int] -> [Int]
initialWeights input = let ln = length input
                        in take (ln * ln) $ repeat 0

indexed :: [a] -> [(Int, a)]
indexed = zip [0..]

train :: [Int] -> [Int] -> [Int]
train p = map fn . indexed
  where
    -- I'll be interested in the value when learning multiple patterns
    fn (i, _) = case (ix i) == (iy i) of
      True -> 0
      False -> (x i) * (y i)
    ix = (`div` l)
    iy = (`mod` l)
    x = (p !!) . ix
    y = (p !!) . iy
    l = length p

fwd :: [Int] -> [Int] -> [Int]
fwd w p = map fn ixp
  where
    ixp = indexed p
    -- I'll be interested in the value when learning multiple patterns
    fn (i, _) = signum . sum . zipWith (*) p $ nthRow l i w
    l = length p

nthRow :: Int -> Int -> [Int] -> [Int]
nthRow rs n = fst . splitAt rs . snd . splitAt (n * rs)
