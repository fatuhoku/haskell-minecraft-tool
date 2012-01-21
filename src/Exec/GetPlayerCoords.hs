module Main where

import System.Environment
import Data.Binary
import qualified Text.Show.Pretty as Pr
import Text.Printf
import Chunk
import NBTExtras
import Region
import Types
import Level
import FileIO

-- We will need to solve the problem of displaying a m x n image
-- directly on top of a player.
--
-- This will involve dealing with overlapping multiple regions.
--
-- We generate a list of commands to execute on each region file by
--  - generating a list of edit commands: solving the problem of overlapping regions
--  - group the commands by which file they operate on.
--  - read the array of blocks from the file
--  - execute the commands on the array
--  - write the array of blocks back into the file.
--
-- We also need a function that would execute a CellReplacement on
-- an array.
-- 
-- In the test file, fileL is the lazy file handle; we can force the whole decompression to
-- occur by doing B.pack . B.unpack $ fileL.

testWorld = "worlds/testworld"

main = do
  args <- getArgs
  putStrLn $ "Got past getArgs"
  let dir = case args of
              [] -> testWorld
              (x:_) -> x

  (Level nbt) <- decodeFile $ getLevelPath dir :: IO Level
  let coord = getPlayerCoords nbt
  putStrLn $ "---------------------------------"
  putStrLn $ "- Pretty printing NBT structure -" 
  putStrLn $ "---------------------------------"
  putStrLn $ Pr.ppShow nbt
  putStrLn $ "-----------------------------"
  putStrLn $ "- Player coordinate         -" 
  putStrLn $ "-----------------------------"
  putStrLn $ show coord
