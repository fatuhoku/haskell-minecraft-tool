module Main where

import Control.Monad
import Data.Binary
import Data.Maybe
import System.Directory
import System.Environment
import System.IO
import qualified Data.ByteString.Lazy as B

import Access
import FileIO
import Chunk
import Coords
import Level
import Region
import Types
import World

-- Take the file path given, read it in as a Region, encode
main :: IO ()
main = do
  args <- getArgs
  if (length args == 1)
    then oneBlock (head args)  
    else do
      putStrLn "Please pass in a file path to a Minecraft world." 
      printUsage

-- I need to test the block setting functionality.

-- I should define a single kind of Region editing code.
-- We must define a path for the test world.
oneBlock :: WorldDirectory -> IO ()
oneBlock dir = do
  -- TODO this function isn't implemented yet!
  -- (World (Level lvl) regions) <- loadWorld dir

  -- Just assume the path is valid.
  (Level level) <- decodeFile (getPath dir (LevelPathParams ())) :: IO Level
  let playerC = fromJust $ getPlayerCoords level

  -- Compute the (chunk)-local cell coordinates
  let (regionC, chunkC, locC) = toMultiCoords playerC

  -- edit the first region?
  let woolId = 35
  let whiteDatum = 0 -- white colour
  let updateBlocks = setBlockId (fiveBlocksAbove locC) woolId
  let updateData = setBlockDatum (fiveBlocksAbove locC) whiteDatum
  let updateRegion = modifyRegion chunkC $ modifyCc $ updateChunk [updateBlocks,updateData]
  edit (getPath dir (RegionPathParams regionC)) updateRegion
  where
    func' f (Region arr) = Region (f arr)
  
fiveBlocksAbove :: CellCoords -> CellCoords 
fiveBlocksAbove (x,z,y) = (x,z,y+5)

printUsage :: IO ()
printUsage = do
  putStr $ "Usage:" 
  putStr $ "  oneblock <<PATH_TO_WORLD>>"

