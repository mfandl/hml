module Main where

import HopfieldMat
import Codec.Picture
import Data.Either
import Control.Applicative

pairs :: [a] -> [b] -> [(a, b)]
pairs as bs = (,) <$> as <*> bs

pixelsAsList :: Image PixelRGB8 -> [Int]
pixelsAsList img@(Image w h _) = map go $ pairs [0..w-1] [0..h-1]
  where go (x, y) = pixelToBinary $ pixelAt img x y
        pixelToBinary (PixelRGB8 r g b) = 0

main :: IO ()
main = do
  img <- readImage "data/test.png"
  let pat = fromRight [] $ pixelsAsList . convertRGB8 <$> img
  putStrLn . show $ length pat
