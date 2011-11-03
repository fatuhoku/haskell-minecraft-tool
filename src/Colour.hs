module Colour where

import Data.List.Extras.Argmax
import Data.Array.IArray
import Png
import Types

-- Map the given colour onto the closest WoolColour.
quantize :: Rgba -> WoolColour
quantize c = fst $ argmin (flip dist c . snd) woolColours

-- a `dist` b is the distance from a to b (this is b-a)
dist :: Rgba -> Rgba -> Double
dist a b = mag $ b `minus` a

-- Computes the magnitude
mag :: Rgba -> Double
mag (r,g,b,a) = fromIntegral . sum $ map (^2) [r,g,b,a]

minus :: Rgba -> Rgba -> Rgba
minus (r1,g1,b1,a1) (r2,g2,b2,a2) = (r1-r2, g1-g2, b1-b2, a1-a2)

quantizePixArray:: PixArray -> WoolColourArray
quantizePixArray = amap quantize

-- The colours for each of the Wool types are given in the list below:
woolColours :: [(WoolColour, Rgba)]
woolColours = [
  (White    , (221,221,221,0)),
  (LightGray, (158,165,165,0)),
  (Gray     , (66,66,66,0)),
  (Black    , (26,23,23,0)),
  (Red      , (163,44,40,0)),
  (Orange   , (234,127,54,0)),
  (Yellow   , (194,180,28,0)),
  (Lime     , (59,188,47,0)),
  (Green    , (55,77,24,0)),
  (LightBlue, (103,138,211,0)),
  (Cyan     , (39,116,149,0)),
  (Blue     , (38,51,153,0)),
  (Purple   , (129,53,195,0)),
  (Magenta  , (190,75,200,0)),
  (Pink     , (217,131,154,0)),
  (Brown    , (85,51,27,0))
  ]
