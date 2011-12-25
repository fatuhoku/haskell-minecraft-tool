module Main where

import System.IO
import System.Environment
import System.Directory
import qualified Data.ByteString.Lazy as B
import Data.Binary

import FileIO
import Region
import Types

-- Take the file path given, read it in as a Region, do nothing with the region
-- and encode then put the region back!
main :: IO ()
main = do
  args <- getArgs
  if (length args == 2)
    then
      let [worldPath, regionCStr] = args
          regionC = read regionCStr
          idRegion = id :: Region -> Region
      in editFile (getPath worldPath (RegionPathParams regionC)) idRegion
    else do
      putStrLn "Please pass in a file path and the region coordinates." 
      printUsage

printUsage :: IO ()
printUsage = do
  putStr $ "Usage:" 
  putStr $ "  roundtrip <<PATH_TO_WORLD>> (regionX,regionZ)"

