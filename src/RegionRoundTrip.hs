module Main where

import System.IO
import System.Environment
import System.Directory
import qualified Data.ByteString.Lazy as B
import Data.Binary

import Types
import Region

-- Take the file path given, read it in as a Region, encode
main :: IO ()
main = do
  args <- getArgs
  if (length args == 1)
    then
      editRegion (head args) id
    else do
      putStrLn "Please pass in a file path to a Minecraft world." 
      printUsage

printUsage :: IO ()
printUsage = do
  putStr $ "Usage:" 
  putStr $ "  roundtrip <<PATH_TO_WORLD>>"

