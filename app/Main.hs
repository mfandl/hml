module Main where

import HopfieldMat as H
import Numeric.LinearAlgebra
import Codec.Picture
import Data.Either
import Control.Applicative

pixelsAsList :: Image PixelRGB8 -> [Int]
pixelsAsList img@(Image w h _) = go <$> [0..w-1] <*> [0..h-1]
  where go y x = pixelToBinary $ pixelAt img x y
        -- black is 1, everything else is zero
        pixelToBinary (PixelRGB8 r _ _) =
          fromIntegral $ 1 - signum r

listToImage :: Int -> Int -> [Int] -> Image PixelRGB8
listToImage w h d =  generateImage go w h
  where
    -- this is a horrible idea performance wise. yolo
    go x y = let v = d !! (y * w + x)
                 pv = fromIntegral $ 255 * (1 - v)
              in PixelRGB8 pv pv pv

loadPixels :: FilePath -> IO (Vector R)
loadPixels path = do
  img <- readImage "data/test.png"
  let pat = fromRight [] $ pixelsAsList . convertRGB8 <$> img
  let vpat = fromList $ fromIntegral <$> pat
  return vpat


main :: IO ()
main = do
  clean <- loadPixels "data/test.png"
  noisy <- loadPixels "data/test_noisy.png"
  let ws = H.train clean (H.initialWeights (size clean))
  let newImg = H.fwd ws noisy
  savePngImage "data/output_test.png" . ImageRGB8
    . listToImage 25 25 $ round <$> toList newImg
