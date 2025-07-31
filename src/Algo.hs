{-# LANGUAGE GHC2021 #-}

module Algo where

import Data.List
import System.Random

newtype Pixel a = Pixel (a, a, a) deriving (Eq, Show, Functor)
newtype Image a = Image [Pixel a] deriving (Show, Functor)
newtype Video a = Video [Image a] deriving (Show)

mapTriple f (x,y,z) = (f x, f y, f z)
combineTriple (x,y,z) (x', y', z') = ((x,x'), (y,y'), (z,z'))

amplification = 32.0
dimming = 0.5
sampleCount = 6

memad x prev = round $ dimming * x + amplification * (x - prev)

intensity :: [Pixel Int] -> Pixel Int
intensity ((Pixel rgb) : prev) =
  let prev' = unzip3 (fmap fromPixel prev)
      sums = mapTriple (fromIntegral .  sum) prev'
      rgbFrac =  mapTriple fromIntegral rgb :: (Float, Float, Float)
      rgb' = mapTriple algo (combineTriple rgbFrac sums)
      in Pixel rgb'
  where
    fromPixel (Pixel p) = p
    algo = uncurry memad

pixelGroups :: [Image a] -> [[Pixel a]]
pixelGroups = transpose . fmap pixels
  where pixels (Image img) = img

applyFilter ::  ([Pixel a] -> Pixel a) -> [Image a] -> Image a
applyFilter f frames =
  let pixels = f <$> pixelGroups frames
  in Image pixels

amplify :: Video Int -> Video Int
amplify (Video v) =
  let v' = (applyFilter intensity . take sampleCount <$> tails v)
  in Video v'

generateVideo :: IO (Video Int)
generateVideo = do
  frames <- replicateM 60  generateFrame
  return Video frames
  where
    generateFrame = replicateM 64 generatePixel
    generatePixel = do
      r <- randomIO
      g <- randomIO
      b <- randomIO
      return Pixel (r, g, b)
