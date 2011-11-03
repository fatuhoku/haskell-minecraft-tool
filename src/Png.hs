module Png where

import Debug.Trace
import Data.Tuple.HT
import qualified Data.ByteString.Lazy as B
import Data.Array
import Data.List
import Data.Binary
import Data.Binary.Get
import Data.Colour.SRGB

import Codec.BMP
import Graphics.GD.ByteString.Lazy

type Rgba = (Int,Int,Int,Int) 
type PixArray = Array (Int,Int) Rgba

type Dims = (Int, Int)

-- Compute the average of a list of RGBA values. Simply apply average to 
-- each component.
average :: PixArray -> Rgba
average pixArray = app4 avg $ unzip4 pixList
  where
    pixList = elems pixArray
    len = length pixList
    avg ls = sum ls `div` len
    app4 f = \(a,b,c,d) -> (f a, f b, f c, f d)

-- Calls getPixel for all the pixels in the range. this is essentially
parseImage :: Image -> IO PixArray
parseImage image = do
  dims <- imageSize image
  let (w,h) = dims `ftrace` (" ... image dimensions are " ++ show dims)
  let topLeft = (0,0)
  let bottomRight = (w-1,h-1)
  pixList <- sequence $ zipWith getPixel (indices dims) (repeat image)
  return $ listArray (topLeft,bottomRight) $ map toRGBA pixList
  where
    -- listArray works on index order
    indices (w,h) = [(x,y) | x <- [0..w-1], y <- [0..h-1]]
    ftrace = flip trace
    

-- Split the array of Rgba values into separate arrays along the width of the
-- array. surely, it is the width that we map...
splitPixArray :: Int -> PixArray -> [PixArray]
splitPixArray n orig = do
  s <- [0..n-1]
  return $ ixmap (low,high) (\(x,y) -> (x+s*nw,y)) orig
  where
    (_,(w1,h1)) = bounds orig -- w1 == width-1, h1 == height-1
    nw = (w1+1) `div` n
    high = (nw-1,h1)
    low = (0,0)

-- Returns a list of average Rgba values for each section of the image.
findAverages :: FilePath -> IO [Rgba]
findAverages fp = do
  pixArray <- withImage (loadPngFile fp) parseImage
  return $ map average $ splitPixArray 16 pixArray
