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
  if (length args == 1)
    then oneBlock (head args)  
    else do
      putStrLn "Please pass in a file path to a Minecraft world." 
      printUsage

-- I need to test the block setting functionality.
--
-- TODO This application will require the most basic accessing code that
-- modifies a world. This means that the modifications to the world wi

-- I should define a single kind of Region editing code.
-- We must define a path for the test world.
oneBlock :: WorldDirectory -> IO ()
oneBlock dir = do
  -- Validate the directory structure.
  valid <- validateMinecraftDirectoryStructure dir
  when (not valid) . error $ "The given directory " ++ dir ++ " is not a valid Minecraft world."
  
  -- TODO this function isn't implemented yet!
  -- (World (Level lvl) regions) <- loadWorld dir

  -- Just assume the path is valid.
  putStrLn "Reading level..."
  (Level level) <- decodeFile $ getLevelPath dir :: IO Level
  let playerC = fromJust $ getPlayerCoords level

  -- Compute the global region, chunk and chunk-local cell coordinates
  let (regionC, chunkC, locC) = (toMultiCoords.playerToCellCoords) playerC
  putStrLn "Extracted coordinates..."
  putStrLn $ "  player: " ++ show playerC
  putStrLn $ "  region: " ++ show regionC
  putStrLn $ "  chunk: " ++ show chunkC
  putStrLn $ "  local: " ++ show locC

  -- edit the first region?
  let woolId = 35    -- wool id
  let whiteDatum = 0 -- white colour
  let updateBlocks = setBlockId (fiveBlocksAbove locC) woolId
  let updateData = setBlockDatum (fiveBlocksAbove locC) whiteDatum

  putStrLn "Updating region..."
  let updateRegion = modifyRegion chunkC $ modifyCc $ updateChunk [updateBlocks,updateData]
  edit (getRegionPath dir regionC) updateRegion

  putStrLn "Done!"
  where
    func' f (Region arr) = Region (f arr)
  
fiveBlocksAbove :: CellCoords -> CellCoords 
fiveBlocksAbove (x,z,y) = (x,z,y+5)

validateMinecraftDirectoryStructure dir = do
  levelDatPresent <- doesFileExist $ getLevelPath dir
  regionDirectoryPresent <- doesDirectoryExist $ getRegionDir dir
  return $ levelDatPresent && regionDirectoryPresent

printUsage :: IO ()
printUsage = do
  putStr $ "Usage:" 
  putStr $ "  oneblock <<PATH_TO_WORLD>>"
