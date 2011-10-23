import Data.NBT

import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zlib as Zlib

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as UTF8 ( fromString, toString )
import Data.Binary -- ( Binary (..), decode, encode )
import Data.Binary.Get
import Data.List
import Data.Maybe

import qualified Text.Show.Pretty as Pr
import Text.Printf

import Control.Applicative
import Control.Monad

import System.Directory
import System.IO

data Region = Region

type RegionCoords = (Int, Int)
type ChunkCoords = (Int, Int)
type CellCoords = (Int, Int)
type SavedGameDirectory = FilePath
type BlockType = String

-- A cell replacement represents the change of blocktype at some position
-- in the game world to to the specified BlockType.
data CellReplacement = CR { cell ::  CellCoords, blockId :: BlockType }

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
main = do
  dec <- loadLevel "worlds/testworld"
  let coord = getPlayerCoords dec
  putStrLn $ "-----------------------------"
  putStrLn $ "- Pretty printing structure -" 
  putStrLn $ "-----------------------------"
  putStrLn $ Pr.ppShow dec
  putStrLn $ "-----------------------------"
  putStrLn $ "- Player coordinate         -" 
  putStrLn $ "-----------------------------"
  putStrLn $ show coord

-- Loads a level.dat file given the saved game directory.
loadLevel :: SavedGameDirectory -> IO NBT
loadLevel dir = do
  let fn = dir ++ "/level.dat"
  fileL <- GZip.decompress <$> B.readFile fn
  let file = B.pack (B.unpack fileL)
      dec = (decode file :: NBT)
      enc = encode dec
  return dec

loadRegion :: SavedGameDirectory -> RegionCoords -> IO NBT
loadRegion dir (x,z) = do
  let fn = printf "r.%d.%d.mcr" x z :: String
  let block = toInteger $ 4*(x+z*32) :: Integer
  withFile (printf "%s/%s" dir fn) WriteMode $ \file -> do
    hSeek file AbsoluteSeek block
    header <- B.hGet file 5
    let (byteCount, compression) = runGet deserializeChunkDataHeader header
    zChunkData <- B.hGet file (fromIntegral byteCount-1)
    let chunkData = decompressWith compression
    let chunkNbt = (decode chunkData :: NBT)
    return EndTag
  where
    deserializeChunkDataHeader :: Get (Word32,Word8)
    deserializeChunkDataHeader = do
      byteCount <- getWord32be
      compressionType <- getWord8
      return (byteCount,compressionType)
    decompressWith compression = case compression of
      1 -> GZip.decompress
      2 -> Zlib.decompress
      _ -> error $ "Unsupported compression type: " ++ show compression
    

--  The relevant Python code for the above is:
--  block = 
--    mcrfile.seek(block)
--    offset, length = unpack(">IB", "\0"+mcrfile.read(4))
--    if offset:
--        mcrfile.seek(offset*4096)
--        bytecount, compression_type = unpack(
--                ">IB", mcrfile.read(5))
--        data = mcrfile.read(bytecount-1)
--        decompressed = decompress(data)
--        nbtfile = NBTFile(buffer=StringIO(decompressed))
--        return nbtfile
--    else:
--        return None

-- Let's write code to put a white wool block 5 squares above the players'
-- head. fn points to a filepath.
-- - identify the exact region in which the player stands
-- - Read the file and seek to the appropriate chunk
putWool5MetresAbovePlayersHead :: FilePath -> IO ()
putWool5MetresAbovePlayersHead fn = do
  dec <- loadLevel "worlds/testworld/level.dat"
  let coord = getPlayerCoords dec
  -- let pcoord = toRegionCoords . toChunkCoords $ coord
  return () -- TODO


-- The way to recover the player's position (Spawn{X,Y,Z} coordinates)
-- from the saved game file is as follows:
--
-- Read in the level.dat
--     ! See
--     ! http://www.minecraftwiki.net/wiki/Alpha_Level_Format
--     - The path to this is
--       (C) _ / (C) Data / (C) Player / (I) Spawn{X,Y,Z}
--     - Confusingly, there is also a world spawn point
--       (C) _ / (C) Data / (I) Spawn{X,Y,Z}
-- 
-- Then, we determine what blocks we will need to render to by using a
-- rectangle class.
 
-- TODO Use a Zipper for the NBT structure instead of a hard-coded path.
getPlayerCoords :: NBT -> (Int, Int, Int)
getPlayerCoords (CompoundTag (Just "" ) tags) =
  let [(CompoundTag (Just "Data") dtags)] = tags in
  case findPlayerTag dtags of
    Nothing -> error "Player tag not found."
    Just ptag -> let ptagContents = compoundContents ptag in
      let [x,y,z] = map getInt $ filter (isPrefixOf "Spawn" . fromJust . getName)
                      $ filter (isJust . getName) ptagContents
      in (x,y,z)
  where
    findPlayerTag = find isPlayerTag
    isPlayerTag (CompoundTag (Just "Player") _) = True
    isPlayerTag _ = False
getPlayerCoords _ = error "Invalid level.dat NBT: does not begin with Data CompoundTag"

readRegionFile :: FilePath -> RegionCoords -> IO Region 
readRegionFile = undefined

toRegionCoords :: ChunkCoords -> RegionCoords
toRegionCoords (x,z) = (chunkToRegion x, chunkToRegion z)
  where
    chunkToRegion n = floor (fromIntegral n/32.0)

toChunkCoords :: CellCoords -> ChunkCoords
toChunkCoords (x,z) = (cellToChunk x, cellToChunk z)
  where
    cellToChunk n = floor (fromIntegral n/16.0)

-- We can express the boundary positions of the rectangle ...

getName :: NBT -> Maybe String
getName EndTag = Nothing
getName (ByteTag      name _) = name
getName (ShortTag     name _) = name
getName (IntTag       name _) = name
getName (LongTag      name _) = name
getName (FloatTag     name _) = name
getName (DoubleTag    name _) = name
getName (ByteArrayTag name _ _) = name
getName (StringTag    name _ _) = name
getName (ListTag      name _ _ _) = name
getName (CompoundTag  name _) = name

getInt (IntTag _ n) = fromIntegral n
getInt _ = undefined

compoundContents (CompoundTag _ contents) = contents
compoundContents _ = error "Not CompoundTag"
