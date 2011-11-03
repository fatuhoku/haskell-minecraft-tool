module Colour where

import Types

-- The colours for each of the Wool types are given in the list below:
woolColours :: WoolColour -> Rgba
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
