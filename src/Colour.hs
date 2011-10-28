module Colour where

import Types

-- The colours for each of the Wool types are given in the list below:
woolColours :: WoolColour -> Rgba
woolColours = [
  (White    , (221,221,221,0)),
  (Orange   , (158,165,165,0)),
  (Magenta  , (66,66,66,0)),
  (LightBlue, (26,23,23,0)),
  (Yellow   , (163,44,40,0)),
  (Lime     , (234,127,54,0)),
  (Pink     , (194,180,28,0)),
  (Gray     , (59,188,47,0)),
  (LightGray, (55,77,24,0)),
  (Cyan     , (103,138,211,0)),
  (Purple   , (39,116,149,0)),
  (Blue     , (38,51,153,0)),
  (Brown    , (129,53,195,0)),
  (Green    , (190,75,200,0)),
  (Red      , (217,131,154,0)),
  (Black    , (85,51,27,0))
  ]
