module Main where

{----------------------}
{- MinecraftPngImport -}
{----------------------}
-- This code will, given a filepath to a PNG image,
-- import the image into the Minecraft world, placing the images' pixels as
-- wool blocks 5 cells above the player's head. 
-- The directory of the world is taken from the arguments.
-- This executable showcases two functionalities:
-- - the ability to read and process png files and extra pixel data;
-- - to compose this with the previously written OneWoolBlock program to make
-- the effect I envisaged at the very beginning.

import Control.Arrow
import Control.Monad
import Data.Binary
import Data.Maybe
import System.Directory
import System.Environment
import System.IO
import qualified Data.ByteString.Lazy as B

import Access
import Block
import Colour
import Chunk
import Coords
import Devil
import FileIO
import Level
import Region
import Types
import World
import Data.Array.IArray
import Control.Arrow
import Data.List
import Data.Function
import Data.Tuple.HT

{-
 - TODO There is a very obvious failure case when running this program.
 - When the Region goes negative, offsets are calculated wrong and things
 - blow up.
 -
 - Reading level...
 - Extracted coordinates...
 -   player: (-60,177,75)
 -   region: (-1,0)
 -   chunk: (-4,11)
 -   local: (4,1,75)
 - Updating region...
 -
 - -}
-- Take the file path given, read it in as a Region, encode
main :: IO ()
main = do
  args <- getArgs
  if (length args == 2)
    then let [png,world] = args in minecraftPngImport png world
    else do
      putStrLn "Please pass in 1) path to a png image and 2) a path to a Minecraft world." 
      printUsage

-- I need to test the block setting functionality.
--
-- TODO This application will require the most basic accessing code that
-- modifies a world. This means that the modifications to the world wi

-- Get the image data out of the png, quantise into wool blocks
minecraftPngImport :: FilePath -> WorldDirectory -> IO ()
minecraftPngImport png dir = do
  image <- readPngImageData png
  let woolImage = amap quantize image
  putImage woolImage dir

type WoolColours = Array (Int,Int) WoolColour
putImage :: WoolColours -> WorldDirectory -> IO ()
putImage woolColours dir = do
  -- Validate the directory structure.
  valid <- validateMinecraftDirectoryStructure dir
  when (not valid) . error $ "The given directory " ++ dir ++ " is not a valid Minecraft world."

  -- Compute wool blocks which we will set.
  let woolBlocks = amap (Block (toBlockId Wool).toDataValue) woolColours
  
  -- we offset the Just assume the path is valid.
  putStrLn "Reading level..."
  (Level level) <- decodeFile $ getLevelPath dir :: IO Level
  let playerC = fromJust $ getPlayerCoords level

  -- Compute the global region, chunk and chunk-local cell coordinates
  -- Let us produce a 'Block'
  let (px,pz,py) = playerToCellCoords playerC
  putStrLn "Extracted coordinates..."
  putStrLn $ "  player: " ++ show playerC
  
  -- Offset the entire image by the playerCell.
  -- Here's the mapping of cell coordinates onto the image coordinates.
  let (minPix,maxPix) = bounds woolBlocks
  let offset (x,y) = (x+px,py+5,y+pz)
  let offset' (cx,cz,cy) = (cx-px,cz-pz)
  let worldImage = ixmap (offset minPix, offset maxPix) offset' woolBlocks

  -- Now take the associations from the world image and work with it.
  undefined


modifyWorld :: WorldDirectory -> [(CellCoords, Block)] -> IO ()
modifyWorld dir changes = do
  -- This will be of the form (CellCoords, Block)
  let updateData = map (first toHierarchicalCoords) changes :: [((RegionCoords, ChunkCoords, LocalCoords),Block)]
  let regionChanges = groupBy ((==) `on` (fst3.fst)) updateData
  let regionNChunkChanges = map (groupBy ((==) `on` (snd3.fst))) regionChanges

  -- We group by region, and within it, group by chunk.
  -- We first annotate every batch of chunk updates with the chunk coordinate.


  let something = map (convertChunkUpdates.map convertBlockUpdates) regionNChunkChanges

  -- Compute from global cell reference
  -- putStrLn $ putStrLn "Calculated coordinates..."
  -- putStrLn $ "  region: " ++ show regionC
  -- putStrLn $ "  chunk: " ++ show chunkC
  -- putStrLn $ "  local: " ++ show locC

  -- Update all the regions.
  -- editFile (getRegionPath dir regionC) $ buildRegionUpdate
  putStrLn "Done!"

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
  blockIdUpdates $ map (second blockId) changes,
  blockDataUpdates $ map (second blockDatum) changes
  ]


-- within each chunk update function, we update both block and data ids.
-- For the chunk updates to be efficient, the block updates and data updates
-- should be done in bulk ([(CellCoord, Value)])
-- Let's do the bulk update thing first.
--
-- An alternative view of the World would be to use multi-dimensional arrays
-- to locate CompressedChunks.
-- What would be cool is if
-- (RegionCoords,ChunkCoords)....

  
fiveBlocksAbove :: CellCoords -> CellCoords 
fiveBlocksAbove (x,z,y) = (x,z,y+5)

validateMinecraftDirectoryStructure dir = do
  levelDatPresent <- doesFileExist $ getLevelPath dir
  regionDirectoryPresent <- doesDirectoryExist $ getRegionDir dir
  return $ levelDatPresent && regionDirectoryPresent

printUsage :: IO ()
printUsage = do
  putStr $ "Usage:" 
  putStr $ "  minecraft-png-import <<PATH_TO_WORLD>>"
