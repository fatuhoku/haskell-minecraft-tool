module Region where
{- Reading and Writing Regions
 -
 - This file adds support for reading and writing Region files.
 - Reading involves parsing of the region file format, and presenting the
 - region as a <list of chunks>.
 -
 - This is implemented as a Binary instance, providing the encode and decode
 - functions.
 - -}
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zlib as Zlib

import Text.Parsec.ByteString
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as UTF8 ( fromString, toString )
import Data.Binary
import Data.Binary.Get
import Data.Word
import Data.Word.Odd
import Data.List
import Data.Maybe
import Data.NBT
import Data.Bits
import Data.Array

import qualified Text.Show.Pretty as Pr
import Text.Printf

import Control.Applicative
import Control.Monad

import System.Directory
import System.IO

import Test.LazySmallCheck

import Types
import Data.Int

-- Kilobytes to Bytes
kB = 1024

-- Constant sizes
locationsFieldSize = 4*kB
timestampsFieldSize = 4*kB
regionFileHeaderSize = 8*kB

-- There are 32x32 chunks in a region.
numChunksInRow = 32
numChunksInCol = 32

-- Region binary instance must provide a get and put functions
instance Binary Region where
  get = getRegion
  put = putRegion

type Location = Int64

-- Read in a region given region coordinates.
loadRegion :: SavedGameDirectory -> RegionCoords -> IO Region
loadRegion directory (x,z) = do
  let filename = printf "r.%d.%d.mcr" x z :: String
  decodeFile (printf "%s/%s" directory filename)

-- TODO Modifying block data (especially adding a lot of detail) 
-- can cause an inflation in the size of the chunk data. When stored again
-- in the region file, this may mess up offsets in the header. 
-- The entire region file should be read, understood, before its chunks are
-- modified and the file as a whole re-written out into the file. 
saveRegion :: SavedGameDirectory -> RegionCoords -> NBT -> IO ()
saveRegion dir (x,z) = undefined

putRegion :: Region -> Put
putRegion bs = undefined

-- For random access, I will not need to know how many sectors to read (this is
-- a low level detail) so I'll just read a Word32 and right-shift the sector
-- count away.
getRegion :: Get Region
getRegion = do
  (locations,timestamps) <- getRegionFileHeader
  chunks <- getChunkData locations timestamps
  let arrayMin = (0,0)
  let arrayMax = (numChunksInRow, numChunksInCol)
  let arrayBounds = (arrayMin, arrayMax)
  let indices = [(x,z) | z <- [0..numChunksInCol-1], x <- [0..numChunksInRow-1]]
  return $ Region $ array arrayBounds (zip indices chunks)
  where

    getRepeatedly = sequence . repeat

    -- Read the region file header and return a list of chunk locations along
    -- with their chunks coordinates.
    getRegionFileHeader :: Get ([Location],[Timestamp])
    getRegionFileHeader = do
      locationsBlock <- getLazyByteString locationsFieldSize
      let locations = runGet (getRepeatedly getChunkLocation) =<< return locationsBlock
      timestampsBlock <- getLazyByteString timestampsFieldSize
      let timestamps = runGet (getRepeatedly getTimestamp) =<< return timestampsBlock
      return (map (4*kB*) locations,timestamps)
      
    -- We will get the 'offset' part of the Chunk location descriptor only.
    -- The sector count field is discarded.
    getChunkLocation :: Get Int64
    getChunkLocation = return . fromIntegral . (`shiftR` 8) =<< getWord32be

    getTimestamp :: Get Timestamp
    getTimestamp = getWord32be

    -- We essentially need to transform the chunk locations into
    -- Get NBT objects. Further, with that, we combine Get NBT and Timestamp to
    -- produce Get Chunk objects.
    getChunkData :: [Location] -> [Timestamp] -> Get [Chunk]
    getChunkData locs times = do
      nbts <- mapM getChunk locs
      return $ zipWith Chunk nbts times

    -- Converts a ByteString representing "Chunk Data"
    -- to a parsed NBT of that chunk.
    getChunk :: Location -> Get NBT
    getChunk loc = do
      bytesRead >>= \br -> uncheckedSkip (loc - fromIntegral br)
      (byteCount, zipMethod) <- getChunkMeta
      compressedChunkData <- getLazyByteString (fromIntegral byteCount-1)
      return $ decode . decompressWith zipMethod $ compressedChunkData 
      where
        getChunkMeta = do
          byteCount <- getWord32be
          compressionType <- getWord8
          return (byteCount,compressionType)

        decompressWith compression = case compression of
          1 -> GZip.decompress
          2 -> Zlib.decompress
          _ -> error $ "Unsupported compression type: " ++ show compression

{- SmallCheck property -}
prop_encDec region = decode (encode region) == region
