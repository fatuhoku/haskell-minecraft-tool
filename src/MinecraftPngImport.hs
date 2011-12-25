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
import Data.Array.IArray
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

type WoolColours = Array (Int,Int) WoolColour

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
  let offset (x,y) = (x+px,y+pz,py+5)
  let offset' (cx,cz,cy) = (cx-px,cz-pz)
  let worldImage = ixmap (offset minPix, offset maxPix) offset' woolBlocks

  putStr $ "The bounds for the image are:"
  putStrLn $ show (minPix,maxPix)
  putStr $ "The bounds for the image in the world are:"
  putStrLn $ show $ bounds worldImage

  let changes = assocs worldImage
  putStrLn $ show changes

  -- Now take the associations from the world image and work with it.
  performWorldUpdate dir changes
  putStrLn "Done!"

printUsage :: IO ()
printUsage = do
  putStr $ "Usage:" 
  putStr $ "  minecraft-png-import <<PATH_TO_WORLD>>"
