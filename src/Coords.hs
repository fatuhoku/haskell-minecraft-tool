module Coords where

{- This module contains constant dimensions that Minecraft uses, type
 - definitions for the coordinate systems used, and code that translates between
 - these coordinate systems. -}

type ChunkIndex = Int -- This is a bit outdated...

-- These are constant dimensions
chunkSizeX = 16
chunkSizeY = 128
chunkSizeZ = 16

numCellsInChunk = chunkSizeX * chunkSizeY * chunkSizeZ

type X = Int -- X coordinate type
type Y = Int -- Z coordinate type
type Z = Int -- Z coordinate type
type RegionCoords = (X, Z)
type ChunkCoords = (X, Z)
type CellCoords = (X, Z, Y)
type LocalCoords = CellCoords

-- Translation between coordinate systems...

-- Transform a (global) CellCoord into a triple
-- (RegionCorod, ChunkCoord, LocalCoord)
toMultiCoords :: CellCoords -> (RegionCoords, ChunkCoords, LocalCoords)
toMultiCoords c = (regionCoords, chunkCoords, localCoords)
  where
    regionCoords = (toRegionCoords.toChunkCoords) c
    chunkCoords = toChunkCoords c
    localCoords = toLocalCoords c

-- Convert from chunk coordinates to region coordinates
toRegionCoords :: ChunkCoords -> RegionCoords
toRegionCoords (x,z) = (chunkToRegion x, chunkToRegion z)
  where
    chunkToRegion n = floor (fromIntegral n/32.0)


-- Convert from cell coordinates to chunk coordinates
toChunkCoords :: CellCoords -> ChunkCoords
toChunkCoords (x,z,_) = (cellToChunk x, cellToChunk z)
  where
    cellToChunk n = floor (fromIntegral n/16.0)

-- Transforms a global cell coordinate into a CellCoordinate local to a single
-- chunk. This is really just modding by chunk... The Y coordinate is unchanged
toLocalCoords :: CellCoords -> LocalCoords
toLocalCoords (x,z,y) = (x `mod` chunkSizeX, z `mod` chunkSizeZ, y)

-- Find index of byte in bytestring that represents given cell coordinate.
toChunkIndex :: CellCoords -> ChunkIndex
toChunkIndex (x,z,y) = y + z * chunkSizeY + x * chunkSizeY * chunkSizeZ

