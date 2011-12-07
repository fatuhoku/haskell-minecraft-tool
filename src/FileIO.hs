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
import Level
import Region
import Types
import Types
import World
import System.FilePath.Posix

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
  edit :: FilePath -> (a -> a) -> IO ()
  edit file f = do
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
