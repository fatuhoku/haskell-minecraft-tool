module World where

import Data.Array
import Coords
import Level
import Region
import Types

{- Reading and Writing The abstraction for a Minecraft world is simply a directory at the moment.
 - TODO Make world a lazy Array of Regions. We make modifications onto a world. -}
type WorldDirectory = FilePath
data World = World Level (Array RegionCoords Region)

{- WORLD EDIT FUNCTIONS -}

-- Perform a side-effectual update of a Minecraft world.
-- The world object itself is built from 
withWorld :: WorldDirectory -> (World -> World) -> IO ()
withWorld dir f = loadWorld dir >>= saveWorld dir . f

-- Load and saving worlds.
loadWorld dir = undefined
saveWorld dir = undefined
