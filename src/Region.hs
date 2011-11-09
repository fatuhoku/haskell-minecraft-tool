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
import Data.Array.IArray (amap)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.Function
import Data.Int
import Data.List
import Data.List.Ordered
import Data.Maybe
import Data.NBT
import Data.Word
import Data.Word.Odd
import Debug.Trace

import qualified Text.Show.Pretty as Pr
import Text.Printf

import Control.Applicative
import Control.Monad

import System.Directory
import System.IO

import Types
import Utils

-- Kilobytes to Bytes, and we define a 'sector' to be 4KiB
kB = 1024
sector = 4*kB

-- The header is 8kB in size
headerSizeInSectors = 2

-- There are 32x32 chunks in a region.
numChunksInRow = 32
numChunksInCol = 32
numChunksInRegion = numChunksInRow * numChunksInCol 

-- Region binary instance must provide a get and put functions
instance Binary Region where
  get = getRegion
  put = putRegion

type Location = Int64

withRegion :: WorldDirectory -> RegionCoords -> (Region -> Region) -> IO ()
withRegion directory coords trans = do
  region <- loadRegion directory coords
  saveRegion directory coords (trans region)

-- Read in a region given region coordinates.
loadRegion :: WorldDirectory -> RegionCoords -> IO Region
loadRegion directory (x,z) = do
  let filename = printf "r.%d.%d.mcr" x z :: String
  decodeFile (printf "%s/region/%s" directory filename)

saveRegion :: WorldDirectory -> RegionCoords -> Region -> IO ()
saveRegion directory (x,z) region = do
  let filename = printf "r.%d.%d.mcr" x z :: String
  encodeFile (printf "%s/region/%s" directory filename) region

-- TODO Modifying block data (especially adding a lot of detail) 
-- can cause an inflation in the size of the chunk data. When stored again
-- in the region file, this may mess up offsets in the header. 
-- The entire region file should be read, understood, before its chunks are
-- modified and the file as a whole re-written out into the file. 

-- | Returns an action in the Put monad that encodes a Region as a ByteString
-- When writing out regions, the locations need not be contiguous. Minecraft
-- shouldn't care about the actual locations, just that the locations header
-- reports the correct bytes.
-- There are several components to writing a chunk:
--    necessary info: (ByteString [compressed NBT],compressed size) 
--
-- - For all associations array (in index order)   Array ((X,Z), Maybe CompressedChunk)
-- - Compress the NBT into a relevant bytestrings...   [((X,Z), Maybe ByteString)] 
--   - Extract the timestamps on each coordinate ... I will need them.
--     We take the liberty of putting a 0 timestamp for null chunks.
--   - Find the size of the resultant compressed bytestring. (B.length)
--   - Compute from this list a list of rounded, and not-rounded lengths.
--     There must be 4KiB alignment in the output!  ()
-- - Scan over this list to produce the list of locations for our region file
--   header
-- - Scan across the sizes of the bytestrings to get the locations header.
-- - Write the locations part of the header.
-- - Write the timestamps part of the header. (largely unchanged!)
-- - Write individual bytestrings in order of their index, aligning at 4KiB
--   boundaries.
putRegion :: Region -> Put
putRegion (Region region) = do

  -- Indices: increase x stepwise before z.
  -- We use the applciative to index the array in altered index order.
  let indices = [(x,z) | z <- [0..numChunksInCol-1], x <- [0..numChunksInRow-1]]

  -- Compute the number of sectors required for each chunk, and therefore
  -- compute the locations.
  -- Retrieve a list of timestamps and write the complete header out.
  let sectorCounts = (amap sectorCountOf region !) <$> indices :: [Word8]
  let locations = map fromIntegral $ scanl (+) headerSizeInSectors sectorCounts :: [Word32]
  let timestamps = (amap timestampOf region !) <$> indices :: [Timestamp]
  putRegionFileHeader locations sectorCounts timestamps
  forM_ (elems region) putCompressedChunkData
  where
    timestampOf :: Maybe CompressedChunk -> Timestamp
    timestampOf (Nothing) = 0
    timestampOf (Just cc) = compressedChunkTimestamp cc

    -- If we simply scan across the list of sectorCounts with addition, we will
    -- get the list of locations, surely.
    sectorCountOf :: Maybe CompressedChunk -> Word8
    sectorCountOf (Nothing) = 0
    sectorCountOf (Just cc) = fromIntegral $ B.length (compressedChunkNbt cc) `divPlus1` 4*kB

    -- putHeader serializes the header out into a bytestring...
    -- This will be index order. 
    putRegionFileHeader :: [Word32] -> [Word8] -> [Timestamp] -> Put
    putRegionFileHeader locations sectionCounts timestamps = do
      zipWithM putLocationEntry locations sectionCounts
      mapM_ putWord32be timestamps

    -- Putting a chunk without data is flushing;
    -- Putting a chunk with compressed data is to write out the data and then
    -- pad it with zeroes.
    putCompressedChunkData :: (Maybe CompressedChunk) -> Put
    putCompressedChunkData (Nothing) = flush -- TODO Ideally I want to do a no-op but...
    putCompressedChunkData chunk@(Just cc) = do
      let sectorSize = 4*kB * fromIntegral (sectorCountOf chunk) :: Int64
      let compressedDataSize = B.length (compressedChunkNbt cc)
      let padding = B.replicate (sectorSize - compressedDataSize) 0
      putLazyByteString (compressedChunkNbt cc)
      putLazyByteString padding 

    -- We need to shiftl and then add the sector size. 
    -- (location::Word48,sectorCount::Word8) entries that go into the locations
    -- header
    putLocationEntry :: Word32 -> Word8 -> Put
    putLocationEntry location sectorCount = do
      put $ shiftL location 8 + fromIntegral sectorCount

-- | @roundToNearest a b @rounds @a@ to the nearest @b@
roundToNearest :: (Integral a) => a ->  a -> a
roundToNearest x n = n * divPlus1 x n

-- | @divPlus1 a b@ essentially finds out the number of @b@s required to fit
-- one @a@ in. 
divPlus1 :: (Integral a) => a -> a -> a
divPlus1 x n = ((x-1) `div` n) + 1

-- | Returns an action in the Get monad that decodes a Region from a ByteString
-- Chunks that have not been generated in the region will have 0 for location.
-- These are represented by ; this means that
-- we have to partition the chunks into a group that do and a group that do not
-- have chunk datas.
getRegion :: Get Region
getRegion = do

  -- Index order, Z-major, X-minor
  let indices = [(x,z) | z <- [0..numChunksInCol-1], x <- [0..numChunksInRow-1]]

  -- Read the file header and associate indices with the read data, partitioning 
  -- chunks that exist (nonNullLocs) on file from those that have not been
  -- generated yet by Minecraft (nullLocs)
  (locations,timestamps) <- getRegionFileHeader

  -- Lift the second component (locations) into Maybe monad
  let mLocations = map (toMaybe (/=0)) locations

  let (nullLocs, nonNullLocs) = partition (isNothing . snd) $ zip indices mLocations

  -- Convert second component from (Maybe Location) to (Maybe CompressedChunk)
  -- Why not have Maybe Location -> Maybe CompressedChunk
  let nullChunks = map (\(i,Nothing) -> (i,Nothing)) nullLocs

  -- Sort chunksList by location of access, ensuring the order of indices
  -- is preserved.
  let (nnIndices, nnSortedLocs) = unzip $ sortBy (compare `on` snd) nonNullLocs
  compressedChunks <- getCompressedChunks (map fromJust nnSortedLocs) timestamps
  let nonNullChunks = zip nnIndices $ map Just compressedChunks

  -- Set up and return the array.
  let arrayMin = (0,0)
  let arrayMax = (numChunksInRow-1, numChunksInCol-1)
  let arrayBounds = (arrayMin, arrayMax)

  -- Processing the nothing chunks ensure that our array is fully defined, even
  -- if they CompressedChunk are sometimes Nothing.
  return $ Region $ array arrayBounds (nullChunks ++ nonNullChunks)
  where
    getTimes n = sequence . replicate n

    -- Read the region file header and return a list of chunk locations along
    -- with their chunks coordinates.
    -- CHECKED! The locations read in are satisfactory.
    getRegionFileHeader :: Get ([Location],[Timestamp])
    getRegionFileHeader = do
      locations <- return . funtrace "Chunk locations: " (printf "%x") . map (4*kB*) =<< getTimes numChunksInRegion getChunkLocation
      timestamps <- getTimes numChunksInRegion getTimestamp
      return (locations,timestamps)
      where
        getChunkLocation :: Get Int64
        getChunkLocation = return . fromIntegral . (`shiftR` 8) =<< getWord32be

        getTimestamp :: Get Timestamp
        getTimestamp = getWord32be

    -- Gets compressed chunk objects. The list of locations must be sorted.
    getCompressedChunks :: [Location] -> [Timestamp] -> Get [CompressedChunk]
    getCompressedChunks locations timestamp = do
      formatsAndCompressedNbts <- mapM getCompressedChunkData locations
      let (compressionFormats, compressedNbts) = unzip $ temptrace formatsAndCompressedNbts
      return $ zipWith3 CompressedChunk compressedNbts compressionFormats timestamp
      where

        temptrace = funtrace "Comp NBT" (\(cf,cn) ->
          printf "(%s,%s)" (show cf) (printf "%x" $ B.length cn :: String))

        -- Converts location into a parsed NBT of that chunk.
        getCompressedChunkData :: Location -> Get (CompressionFormat, B.ByteString)
        getCompressedChunkData loc = do
          bytesRead >>= \br -> skip $ fromIntegral (loc - fromIntegral br) 
          (byteCount, formatId) <- getChunkMeta
          bs                    <- getLazyByteString (fromIntegral byteCount)
          let format = toCompressionFormat formatId 
          return (format, bs)

        getChunkMeta = do
          byteCount <- getWord32be
          compressionType <- getWord8
          return (byteCount,compressionType)

        toCompressionFormat n = case n of
          1 -> GZip
          2 -> Zlib
          _ -> error $ "toCompressionFormat: unsupported compression number: " ++ show n
