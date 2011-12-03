module Main where

import Data.NBT

import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zlib as Zlib

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as UTF8 ( fromString, toString )
import Data.Array
import Data.Binary -- ( Binary (..), decode, encode )
import Data.Binary.Get
import Data.List
import Data.Maybe

import qualified Text.Show.Pretty as Pr
import Text.Printf
import System.IO
import System.Environment

import Control.Applicative
import Control.Monad

import System.Directory
import System.IO

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
  let path = case args of
              [] -> testWorld
              (x:_) -> x

  (Level nbt) <- decodeFile path :: IO Level
  let coord = getPlayerCoords nbt
  putStrLn $ "---------------------------------"
  putStrLn $ "- Pretty printing NBT structure -" 
  putStrLn $ "---------------------------------"
  putStrLn $ Pr.ppShow nbt
  putStrLn $ "-----------------------------"
  putStrLn $ "- Player coordinate         -" 
  putStrLn $ "-----------------------------"
  putStrLn $ show coord
