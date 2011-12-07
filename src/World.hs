module World where

import Data.Array
import Coords
import Level
import Region
import Types

{---------}
{- World -}
{---------}
-- This module provides facilities to read and write into a Minecraft world.
-- The abstraction for a Minecraft world is simply a directory at the moment.
--
-- TODO Alter the abstraction for world: make it a lazy Array of Regions.
-- Changes to the world should be grouped by region, and those changes should be
-- made to the region in one swift batch.
--
-- TODO Make a function // setBlocks :: [(CellCoord,Block)] -> World -> World //
-- that would nice and simply update a bunch of blocks in the world, even if
-- they cross chunk boundaries.
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
