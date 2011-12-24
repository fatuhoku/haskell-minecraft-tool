module Region where

{- Reading, Editing and Writing Region files
 -
 - This source file handles the reading, editing and writing Region files.
 - Reading involves parsing of the region file format, and presenting the
 - region as a <list of chunks>.
 -
 - The Region file is treated as a very specific serialized form of a Region
 - data structure, essentially an Array of CompressedChunks.
 -
 - TODO Could we possible remove the distinction between compressed and
 - uncompressed chunk datas?
 - -}
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zlib as Zlib

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as UTF8 ( fromString, toString )
import Data.Array hiding (indices)
import Data.Array.IArray (amap)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.Function
import Data.Int
import Data.List
import Data.Maybe
import Data.NBT
import Data.Tuple.HT
import Data.Word
import Debug.Trace

import qualified Text.Show.Pretty as Pr
import Text.Printf

import Control.Applicative
import Control.Monad

import Coords
import Types
import Utils

-- Kilobytes to Bytes, and we define a 'sector' to be 4KiB
kB = 1024
sector = 4*kB

-- The header is 8kB in size
headerSizeInSectors = 2

-- Indices: increase x stepwise before z.
-- We use the applciative to index the array in altered index order.
indices = [(x,z) | z <- [0..numChunksInCol-1], x <- [0..numChunksInRow-1]]

-- Region binary instance must provide a get and put functions
instance Binary Region where
  get = getRegion
  put = putRegion

type Location = Int64

-- Fully represents the 8KiB RegionFile's header in the region file.
-- The location is given in 4KiB sectors.
data RegionFileHeader = RegionFileHeader {
  rfHdrLocations :: [Word32], -- wrapping over a Word24 really.
  rfHdrSectorCounts :: [Word8],
  rfHdrTimestamps :: [Timestamp] -- synonym for Word32
  }

instance Binary RegionFileHeader where
  put (RegionFileHeader locs scs tss) =
    -- (loc::Word24,sectorCount::Word8) require some bit-shifting work.
    let putLocationEntry :: Word32 -> Word8 -> Put
        putLocationEntry location sectorCount = do
          putWord32be $ shiftL location 8 .|. fromIntegral sectorCount
    in do
      zipWithM putLocationEntry locs scs
      forM_ tss putWord32be

  -- getting a (loc::Word24,sectorCount::Word8) require some bit-shifting work.
  get = 
    let getLocationEntry :: Get (Word32,Word8)
        getLocationEntry = getWord32be >>= \x -> do
          return (x `shiftR` 8, fromIntegral $ x `mod` (2^8))
        getTimestamp = getWord32be
    in do
      (locs,scs) <- liftM unzip $ replicateM numChunksInRegion getLocationEntry
      tss <- replicateM numChunksInRegion getTimestamp
      return $ RegionFileHeader locs scs tss 

instance Binary CompressionFormat where
  get = getWord8 >>= \n -> return $ g n
    where 
      g 1 = GZip
      g 2 = Zlib
      g x = error $ "get CompressionFormat: unsupported compression number: " ++ show x
  put GZip = putWord8 1
  put Zlib = putWord8 2
  
{- REGION EDIT FUNCTIONS -}

-- Given a region-local chunk coordinate, we update that chunk in our region.
modifyRegion :: ChunkCoords -> (CompressedChunk -> CompressedChunk) -> Region -> Region
modifyRegion coords f (Region arr) =
  let mCompressedChunk = arr ! coords :: Maybe CompressedChunk in
  Region $ arr // [(coords, f <$> mCompressedChunk)]

{- SERIALIZATION FUNCTIONS -}

-- | Returns an action in the Put monad that serializes a Region into a byte
-- string. Chunks are aligned to 4KiB "sectors", and padding is used for space
-- that is not used by chunk data.
putRegion :: Region -> Put
putRegion (Region region) = do
  -- 1) We get a list of compressed Chunks in the required index order.
  let compressedChunks = (region !) <$> indices
  -- 2) We extract from this the sector counts and the timestamps
  let scs = sectorCountOf <$> compressedChunks :: [Word8]
  let tss = timestampOf <$> compressedChunks :: [Timestamp]

  -- 3) Scan over the list to give a tentative list of locations, making sure that we
  -- zero out the locations that represent null chunks (chunks with length zero)
  let tentativeLocs = scanl (+) headerSizeInSectors (map fromIntegral scs) :: [Location]
  let locs = fromIntegral <$> zipWith locationFilter tentativeLocs scs :: [Word32]

  -- 4) Write this out as the file header...
  put $ RegionFileHeader locs scs tss

  -- 5) Then we start putting the chunk data, which works out the appropriate
  -- padding, etc.
  forM_ compressedChunks putCompressedChunkData -- I must dereference it in the apporpriate way!
  where
    -- We will need to set any locations that, after scanning, have a 0 sector
    -- count to zero.
    locationFilter :: Location -> Word8 -> Location
    locationFilter loc sc = if sc == 0 then 0 else loc

    timestampOf :: Maybe CompressedChunk -> Timestamp
    timestampOf (Nothing) = 0
    timestampOf (Just cc) = compressedChunkTimestamp cc

    -- If we simply scan across the list of sectorCounts with addition, we will
    -- get the list of locations, surely.
    sectorCountOf :: Maybe CompressedChunk -> Word8
    sectorCountOf (Nothing) = 0
    sectorCountOf (Just (CompressedChunk {compressedChunkNbt=cNbt})) = fromIntegral $ B.length cNbt `divPlus1` (4*kB)
      
    -- Put the chunk meta (field size and compression type)
    -- Put compressed data, padding the block up with zeroes.
    -- -5 accounts for the bytes used for the header.
    putCompressedChunkData :: (Maybe CompressedChunk) -> Put
    putCompressedChunkData (Nothing) = return ()
    putCompressedChunkData chunk@(Just cc) = do
      let sectorLength = 4*kB * fromIntegral (sectorCountOf chunk) :: Int64
      let compressedData = compressedChunkNbt cc
      let compressedDataSize = B.length compressedData
      let padding = B.replicate (sectorLength - compressedDataSize - 5) 0
      putWord32be $ fromIntegral compressedDataSize
      put $ compressedChunkFormat cc
      mapM_ putLazyByteString [compressedChunkNbt cc,padding] 

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
  -- Read the file header and associate indices with the read data, partitioning 
  -- chunks that exist (nonNullLocs) on file from those that have not been
  -- generated yet by Minecraft (nullLocs)
  RegionFileHeader locations _ timestamps  <- get

  -- Now we do a transformation of two parts:
  -- 1) Lift the locations into maybe by comparison to 0
  -- 2) Convert the locations to byte offsets (Word32 -> Int64)
  let mByteOffsets = fmap (4*kB*) . toMaybe (/=0) . fromIntegral <$> locations

  -- 3) Associate indices and timestamps and then sort, with Nothings floating to top.
  -- sortedIndexedMaybeOffsets (SIMO) is of the form [(array index, mOffset, timestamp)]
  let simo = sortBy (compare `on` snd3) $ zip3 indices mByteOffsets timestamps

  -- 4) Access the file in a sequenced way, extracting Compressed Chunks.
  compressedChunks <- zipWithM getCompressedChunk (snd3 <$> simo) (thd3 <$> simo)
  let indexedCompressedChunks = zip (fst3 <$> simo) compressedChunks

  -- Set up and return the array.
  let arrayMin = (0,0)
  let arrayMax = (numChunksInRow-1, numChunksInCol-1)
  let arrayBounds = (arrayMin, arrayMax)

  -- Processing the nothing chunks ensure that our array is fully defined, even
  -- if they CompressedChunk are sometimes Nothing.
  return . Region $ array arrayBounds indexedCompressedChunks
  where
    -- Gets compressed chunk objects. The list of locations must be sorted.
    getCompressedChunk :: (Maybe Location) -> Timestamp -> Get (Maybe CompressedChunk)
    getCompressedChunk Nothing _ = return Nothing
    getCompressedChunk (Just loc) ts = do
      -- 1) Skip ahead to the location we want.
      bytesRead >>= \br -> skip $ fromIntegral (loc - (fromIntegral br))
      -- 2) Extract its compressed data
      (compFormat, compData) <- getCompressedChunkData loc
      return . Just $ CompressedChunk compData compFormat ts

    -- Reads 5 byte leader (meta) for the chunk, retrieving the length of the compressed
    -- data in bytes as well as the compression format it was compressed with.
    getCompressedChunkData :: Location -> Get (CompressionFormat, B.ByteString)
    getCompressedChunkData 0 = error "I should never be getting chunk data from the 0th location"
    getCompressedChunkData loc =
      let getChunkMeta = liftM2 (,) getWord32be get
      in do
        (byteCount, compressionFormat) <- getChunkMeta
        bs <- getLazyByteString (fromIntegral byteCount)
        return (compressionFormat, bs)

