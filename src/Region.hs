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

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as UTF8 ( fromString, toString )
import Data.Array
import Data.Binary
import Data.Binary.Get
import Data.Bits
import Data.Function
import Data.Int
import Data.List
import Data.List.Ordered
import Data.Maybe
import Data.NBT
import Data.Word
import Data.Word.Odd

import qualified Text.Show.Pretty as Pr
import Text.Printf

import Control.Applicative
import Control.Monad

import System.Directory
import System.IO

import Debug.Trace

import Types

-- Kilobytes to Bytes
kB = 1024

-- There are 32x32 chunks in a region.
numChunksInRow = 32
numChunksInCol = 32
numChunksInRegion = numChunksInRow * numChunksInCol 

-- Region binary instance must provide a get and put functions
instance Binary Region where
  get = getRegion
  put = putRegion

type Location = Int64

-- Read in a region given region coordinates.
loadRegion :: WorldDirectory -> RegionCoords -> IO Region
loadRegion directory (x,z) = do
  let filename = printf "r.%d.%d.mcr" x z :: String
  decodeFile (printf "%s/region/%s" directory filename)

-- TODO Modifying block data (especially adding a lot of detail) 
-- can cause an inflation in the size of the chunk data. When stored again
-- in the region file, this may mess up offsets in the header. 
-- The entire region file should be read, understood, before its chunks are
-- modified and the file as a whole re-written out into the file. 

-- | Returns an action in the Put monad that encodes a Region as a ByteString
putRegion :: Region -> Put
putRegion bs = error "putRegion: not implemented"

-- | Returns an action in the Get monad that decodes a Region from a ByteString
-- Chunks that have not been generated in the region will have 0 for location.
-- These are represented by ; this means that
-- we have to partition the chunks into a group that do and a group that do not
-- have chunk datas.
getRegion :: Get Region
getRegion = do

  -- Read the file header.
  (locations,timestamps) <- getRegionFileHeader

  -- Associate every location with an index, and then partition those that have null
  -- location
  let indices = [(x,z) | z <- [0..numChunksInCol-1], x <- [0..numChunksInRow-1]]
  let (nullChunks, nonNullChunks) = partition ((==0) . snd) $ zip indices locations

  -- Process null chunks by converting zero location to Nothing
  let nothingChunks = map (\(i,0) -> (i,Nothing)) nullChunks `ftrace` (show nonNullChunks)

  -- Process non-null (actual) chunks by reading from compressed data.
  let chunksList = sortBy (compare `on` snd) nonNullChunks
  chunks <- getChunkData (map snd chunksList) timestamps
  let actualChunks = zip (map fst chunksList) $ map Just chunks

  -- Set up and return the array.
  let arrayMin = (0,0)
  let arrayMax = (numChunksInRow, numChunksInCol)
  let arrayBounds = (arrayMin, arrayMax)

  return $ Region $ array arrayBounds (nothingChunks ++ actualChunks)
  where
    ftrace = flip trace

    getTimes n = sequence . replicate n

    -- Read the region file header and return a list of chunk locations along
    -- with their chunks coordinates.
    getRegionFileHeader :: Get ([Location],[Timestamp])
    getRegionFileHeader = do
      locations <- getTimes numChunksInRegion getChunkLocation
      timestamps <- getTimes numChunksInRegion getTimestamp
      return (map (4*kB*) locations,timestamps)
      
    -- We will get the 'offset' part of the Chunk location descriptor only.
    -- The sector count field is discarded.
    getChunkLocation :: Get Int64
    getChunkLocation = return . fromIntegral . (`shiftR` 8) =<< getWord32be

    getTimestamp :: Get Timestamp
    getTimestamp = getWord32be

    -- Get chunks from the current bytestring.
    -- locs must be ordered.
    getChunkData :: [Location] -> [Timestamp] -> Get [Chunk]
    getChunkData locs times = do
      nbts <- mapM getChunk locs
      return $ zipWith Chunk nbts times

    -- Converts a ByteString representing "Chunk Data"
    -- to a parsed NBT of that chunk.
    getChunk :: Location -> Get NBT
    getChunk loc = do
      bytesRead >>= \br -> skip $ fromIntegral (loc - fromIntegral br) 
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
