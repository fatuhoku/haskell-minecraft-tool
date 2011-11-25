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
      performRoundTrip $ head args
    else do
      putStrLn "Please pass in a file path to a Minecraft world." 
      printUsage

-- Move to backup copy; read from backup copy to produce new copy.
performRoundTrip :: FilePath -> IO ()
performRoundTrip file = do
  let backupCopy = file ++ ".bak"
  renameFile file backupCopy
  rd <- openFile backupCopy ReadMode
  wr <- openFile file WriteMode
  contents <- B.hGetContents rd
  let result = encode $ (decode contents :: Region)
  B.hPutStr wr result
  hClose wr
  hClose rd

printUsage :: IO ()
printUsage = do
  putStr $ "Usage:" 
  putStr $ "  roundtrip <<PATH_TO_WORLD>>"

