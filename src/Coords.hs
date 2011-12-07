module Coords where

{---------}
{- Chunk -}
{---------}
-- This module contains constant dimensions that Minecraft uses, type
-- definitions for the coordinate systems used, and code that translates between
-- these coordinate systems.

type X = Int -- X coordinate type
type Y = Int -- Z coordinate type
type Z = Int -- Z coordinate type

-- There are 32x32 chunks in a region.
numChunksInRow = 32   -- number of chunks in a region along X
numChunksInCol = 32   -- number of chunks in a region along Z
numChunksInRegion = numChunksInRow * numChunksInCol 

-- There are 16x128x16 cells in a chunk.
chunkSizeX = 16   -- number of cells in a chunk along X
chunkSizeY = 128  -- number of cells in a chunk along Y
chunkSizeZ = 16   -- number of cells in a chunk along Z
numCellsInChunk = chunkSizeX * chunkSizeY * chunkSizeZ

{- Coordinates types -}

-- Global coordinate types can be converted between one another by means of scaling
-- and downsampling. Notice that player coordinates is different, where Z
-- doesn't come first.
-- We shouldn't consider global chunk coordinates, because it is unhelpful.
-- Instead, we consider global cell and global region coordinates;
-- Then, to work locally within a region, we need region-local chunk
-- coordinates, and to work locally within a chunk, we need chunk-local cell
-- coordinates.
-- TODO Make it clear what coordinates we are talking about.
newtype PlayerCoords = PlayerCoords (X, Y, Z)  -- Global player coordinates
  deriving (Eq, Show)
type CellCoords = (X, Z, Y)    -- Global cell coordinates
type RegionCoords = (X, Z)     -- Global region coordinates
type ChunkCoords = (X, Z)      -- Region-local chunk coordinates (mod)
type LocalCoords = CellCoords  -- Chunk-local cell coordinates (mod)

-- TODO Incoorporate this fact into tests:
-- Chunk at (30, -3) would be in region (0, -1),
-- and one at (70, -30) would be at (2, -1).

{- Translation between coordinate systems... -}

-- Safe conversion between player coordinates (in level.dat) and global cell
-- coordinates
playerToCellCoords :: PlayerCoords -> CellCoords
playerToCellCoords (PlayerCoords (x,y,z)) = (x,z,y)

toMultiCoords :: CellCoords -> (RegionCoords, ChunkCoords, LocalCoords)
toMultiCoords c = (regionCoords, chunkCoords, localCoords)
  where
    regionCoords = toRegionCoords c
    chunkCoords = toChunkCoords c
    localCoords = toLocalCoords c

-- We have to scale the cell value down to chunks (/16) and then
-- scale that down to region (/32).
-- The overall effect is scaling by 512 times.
-- TODO Why not just do it with integers...?
toRegionCoords :: CellCoords -> RegionCoords
toRegionCoords (x,z,_) = (rx, rz)
  where
    (x', z') = (fromIntegral x, fromIntegral z) :: (Double, Double)
    factorX = fromIntegral (chunkSizeX*numChunksInRow) :: Double
    factorZ = fromIntegral (chunkSizeZ*numChunksInCol) :: Double
    rx = floor (x'/factorX)
    rz = floor (z'/factorZ)

-- To get Chunk coordinates, we can just
--  - Get the global chunk coordinates by scaling by chunkSizeX
--  - Mod by numChunksInRow
toChunkCoords :: CellCoords -> ChunkCoords
toChunkCoords (x,z,_) = (cx,cz)
  where
    cx = x `div` chunkSizeX `mod` numChunksInRow
    cz = z `div` chunkSizeZ `mod` numChunksInCol

-- Transforms a global cell coordinate into a CellCoordinate local to a single
-- chunk. This is really just modding by chunk... The Y coordinate is unchanged
toLocalCoords :: CellCoords -> LocalCoords
toLocalCoords (x,z,y) = (x `mod` chunkSizeX, z `mod` chunkSizeZ, y)

-- Find index of byte in bytestring that represents given cell coordinate.
-- toChunkIndex :: CellCoords -> ChunkIndex
-- toChunkIndex (x,z,y) = y + z * chunkSizeY + x * chunkSizeY * chunkSizeZ

