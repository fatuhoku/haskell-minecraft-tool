module Main where

import System.IO
import System.Environment
import System.Directory
import qualified Data.ByteString.Lazy as B
import Data.Binary

import Types
import Region
import Level
import Chunk
import Access

-- Take the file path given, read it in as a Region, encode
main :: IO ()
main = do
  args <- getArgs
  if (length args == 1)
    then oneBlock (head args)  
    else do
      putStrLn "Please pass in a file path to a Minecraft world." 
      printUsage

-- We must define a path for the test world.
oneBlock :: FilePath -> IO ()
oneBlock fp = do
  [path] <- getArgs

  -- Just assume the path is valid.
  dec <- loadLevel path
  let plyrCoords = getPlayerCoords dec

  -- edit the first region?
  let chunkCoords = toChunkCoords plyrCoords
  let regionCoords = toRegionCoords chunkCoords
  let woolId = 35
  let whiteDatum = 0 -- white colour
  let updateBlocks = setBlockId (fiveBlocksAbove plyrCoords) woolId
  let updateData = setBlockDatum (fiveBlocksAbove plyrCoords) whiteDatum
  let updateRegion = modifyRegion chunkCoords $ modifyCc $ updateChunk [updateBlocks,updateData]
  editRegion (regionFilePath path regionCoords) updateRegion
  where
    func' f (Region arr) = Region (f arr)
  
fiveBlocksAbove :: CellCoords -> CellCoords 
fiveBlocksAbove (x,z,y) = (x,z,y+5)

printUsage :: IO ()
printUsage = do
  putStr $ "Usage:" 
  putStr $ "  oneblock <<PATH_TO_WORLD>>"

