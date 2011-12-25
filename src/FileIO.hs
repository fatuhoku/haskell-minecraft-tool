{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
module FileIO where

{----------}
{- FileIO -}
{----------}
-- This module contains all side-effectual functionality required to modify a
-- Minecraft world. A typeclass called FileBacked is used to abstract over types
-- of files found in the Minecraft world.
--
-- Two kinds of things in a Minecraft world are FileBacked:
--  - level.dat (Level) and
--  - r.X.Z.mcr (Region) files.
--
import qualified Data.ByteString.Lazy as B
import Codec.Compression.GZip as GZip
import Codec.Compression.Zlib as Zlib
import Control.Applicative
import Data.Binary
import Data.NBT
import Text.Printf
import System.Directory
import System.IO
import Coords
import Access
import Block
import Chunk
import Level
import Region
import Types
import Types
import World
import System.FilePath.Posix
import Control.Arrow
import Data.List
import Data.Function
import Data.Tuple.HT

-- FileBacked represents a common type of file in the Minecraft world.
-- These files are expected to live under the Minecraft WorldDirectory.
-- Their file locations under this directory can vary by type; the getPath method
-- provides a means for users to calculate such a path rooted at the Minecraft
-- WorldDirectory. Notice PathParam: this associated type allows the user to pass
-- more specific data required to compute the exact path or filename of the file.
-- Edit is a specific implementation of how the file should be edited, given an
-- exact path and an update function.
-- Binary instances are expected. Typical usage for FileBacked data fb are:
--   encodeFile fb
--   decodeFile fb
-- TODO fix the signiture of edit to remove FilePath.
class (Binary a) => FileBacked a where
  data PathParam a
  getPath :: WorldDirectory -> PathParam a -> FilePath
  
  -- Edit copies the original file to a backup file. The backup file is read,
  -- and the new modified version of the file is created.
  editFile :: FilePath -> (a -> a) -> IO ()
  editFile file f = do
    let backup = file ++ ".bak"
    renameFile file backup
    x <- decodeFile backup
    encodeFile file (f x)

-- NBTs are essentially always going to treated the same:
-- it's compressed and stored in a bytestring.
type LevelUnit = () -- for passing no parameters into the reading the level.
instance FileBacked Level where
  data PathParam Level = LevelPathParams LevelUnit
  getPath dir _ = printf "%s/level.dat" dir

instance FileBacked Region where
  data PathParam Region = RegionPathParams RegionCoords
  getPath dir (RegionPathParams (x,z)) =
    let fn = printf "r.%d.%d.mcr" x z :: String in
    printf "%s/region/%s" dir fn

{- WORLD -}

performWorldUpdate :: WorldDirectory -> [(CellCoords, Block)] -> IO ()
performWorldUpdate dir changes = do
  -- This will be of the form (CellCoords, Block)
  let updateData = map (first toHierarchicalCoords) changes :: [((RegionCoords, ChunkCoords, LocalCoords),Block)]
  let regionChanges = groupBy ((==) `on` (fst3.fst)) updateData
  let regionNChunkChanges = map (groupBy ((==) `on` (snd3.fst))) regionChanges
  -- We group by region, and within it, group by chunk.
  let regionUpdates = map (convertChunkUpdates.map convertBlockUpdates) regionNChunkChanges
  mapM_ (uncurry $ performRegionUpdate dir) regionUpdates


performRegionUpdate :: WorldDirectory -> RegionCoords -> (Region -> Region) -> IO ()
performRegionUpdate dir regionC = editFile (getRegionPath dir regionC)

convertBlockUpdates :: [(HierarchicalCoords, Block)] -> ((RegionCoords, ChunkCoords), Chunk -> Chunk)
convertBlockUpdates blockUp@(((r,c,_),_):_) =
    ((r,c), buildChunkUpdate $ map (first getLocalCoords) blockUp)
convertChunkUpdates :: [((RegionCoords,ChunkCoords),Chunk -> Chunk)] -> (RegionCoords, Region -> Region)
convertChunkUpdates chunkUp@(((r,c),_):_) =
    (r, buildRegionUpdate $ map (first snd) chunkUp)


-- There are a number of regions modified [((X,Z), Region -> Region)]
-- ... this can be derived from a list of [((X,Z), Chunk -> Chunk)]
-- that modify chunks; we group them by their region.

-- Given a whole bunch of Chunk updates, we need to apply modify
buildRegionUpdate :: [(ChunkCoords, Chunk -> Chunk)] -> (Region -> Region)
buildRegionUpdate [] = id
buildRegionUpdate ((cc,f):us) = buildRegionUpdate us . modifyRegion cc (liftCc f)

-- Starting from the basics,
-- Similarly, this can be derived from a list of updates to cells
-- [(LocalCoords, Block)]
-- Control.Arrow is very useful here.
buildChunkUpdate :: [(LocalCoords, Block)] -> (Chunk -> Chunk)
buildChunkUpdate changes = updateChunk [
  blockDataUpdates $ map (second blockDatum) changes,
  blockIdUpdates $ map (second blockId) changes
  ]

validateMinecraftDirectoryStructure dir = do
  levelDatPresent <- doesFileExist $ getLevelPath dir
  regionDirectoryPresent <- doesDirectoryExist $ getRegionDir dir
  return $ levelDatPresent && regionDirectoryPresent

{- LEVEL -}

{- REGION -}

-- TODO Iron out the interface for accessing regions and region files
-- The IO should be handled by World, as it knows about the entire structure
-- of the saved file.
-- Wrap a transformation into the region. 
-- withRegion :: WorldDirectory -> RegionCoords -> (Region -> Region) -> IO ()
-- withRegion directory coords trans = do
--   region <- loadRegion directory coords
--   saveRegion directory coords (trans region)

loadRegion :: WorldDirectory -> RegionCoords -> IO Region
loadRegion dir coords = decodeFile $ getRegionPath dir coords

getRegionDir ::  WorldDirectory -> FilePath
getRegionDir dir = dropFileName $ getRegionPath dir (0,0)

getRegionPath ::  WorldDirectory -> RegionCoords -> FilePath
getRegionPath dir coords = getPath dir (RegionPathParams coords)

getLevelPath :: WorldDirectory -> FilePath
getLevelPath dir = getPath dir $ LevelPathParams ()
