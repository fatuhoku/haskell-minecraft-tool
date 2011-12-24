module Devil where

import Codec.Image.DevIL
import System.Environment
import Data.Array.Unboxed hiding (array)
import Data.Array.IArray (array)
import Data.List
import Data.List.Split
import Control.Applicative

type Dims = (Int, Int)
type Rgb = (Word8,Word8,Word8) 
type Rgba = (Word8,Word8,Word8,Word8) 
type DevilImage = UArray (Int, Int, Int) Word8
type Image = Array Dims Rgba

-- This representation is sub-standard. 
-- Conver this int
-- We would like the format to be more like UArray
-- Groupby the third element...
-- We could ixMap (X,Z) -> (X,Z,),())
readPngImageData :: FilePath -> IO Image
readPngImageData path = do
  ilInit
  image <- readImage path
  return $ fromDevilImage image

numChannels = 4

fromDevilImage :: DevilImage -> Image
fromDevilImage devilImage = let bnds = fromDevilBounds $ bounds devilImage in
  array bnds $ map fromDevilToRgb $ splitEvery numChannels $ assocs devilImage

-- Convert from the Devil pixel list format
-- I need non-strict boxed arrays to talk about colours properly :/
fromDevilToRgb [((x,y,_),r),(_,g),(_,b),(_,a)] = ((x,y),(r,g,b,a))
fromDevilBounds ((minX,minY,_),(maxX,maxY,_)) = ((minX,minY),(maxX,maxY))



-- Compute the average of a list of RGBA values. Simply apply average to 
-- each component.
-- average :: Image -> Rgba
-- average image = app4 avg $ unzip4 pixList
--   where
--     pixList = elems image
--     len = length pixList
--     avg ls = sum (map fromIntegral ls) `div` len
--     app4 f = \(a,b,c,d) -> (f a, f b, f c, f d)

-- Split the array of Rgba values into separate arrays along the width of the
-- array. surely, it is the width that we map...
splitImage :: Int -> Image -> [Image]
splitImage n orig = do
  s <- [0..n-1]
  return $ ixmap (low,high) (\(x,y) -> (x+s*nw,y)) orig
  where
    (_,(w1,h1)) = bounds orig -- w1 == width-1, h1 == height-1
    nw = (w1+1) `div` n
    high = (nw-1,h1)
    low = (0,0)
