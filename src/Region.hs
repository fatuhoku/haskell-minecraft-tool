module Region where

import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zlib as Zlib

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as UTF8 ( fromString, toString )
import Data.Binary -- ( Binary (..), decode, encode )
import Data.Binary.Get
import Data.Word.Odd
import Data.List
import Data.Maybe
import Data.NBT

import qualified Text.Show.Pretty as Pr
import Text.Printf

import Control.Applicative
import Control.Monad

import System.Directory
import System.IO

import Test.LazySmallCheck

import Types

-- Kilobytes to Bytes
kB = 1024

-- Constant sizes
locationsFieldSize = 4*kB
timestampsFieldSize = 4*kB
regionFileHeaderSize = 8*kB

-- Read in a region given region coordinates.
loadRegion :: SavedGameDirectory -> RegionCoords -> IO Region
loadRegion dir (x,z) = do
  let fn = printf "r.%d.%d.mcr" x z :: String
  withFile (printf "%s/%s" dir fn) ReadMode $ \file -> do
    -- Read Region file header
    bLoc <- B.hGet file locationsFieldSize
    bTs  <- B.hGet file timestampsFieldSize

    -- Read Region file body
    -- TODO fix the undefined parts.
    -- locations <- runGet (sequence . repeat getChunkLocation) bLoc
    locations <- runGet undefined bLoc
    timeStamps <- runGet undefined bTs
    
    -- We process the locations by parsing every chunk as NBTs.
    chunkNbts <- forM locations $ \(offset, sectorCount) -> do
      let pos = fromIntegral $ offset * 4*kB
      hSeek file AbsoluteSeek pos
      chunkData <- B.hGet file (sectorCount * 4*kB)
      return $ toChunkNbt chunkData

    -- Build the final Region
    return . Region $
      zipWith (\cNbt cTs -> Chunk cNbt cTs) chunkNbts timeStamps
  where
    getChunkLocation :: Get (Word48, Word8)
    getChunkLocation = do
      offset <- undefined -- getWord48be
      sectorCount <- getWord8
      return (offset, sectorCount)

    getTimestamp :: Get Timestamp
    getTimestamp = getWord32be

    -- Converts a ByteString representing "Chunk Data"
    -- to a parsed NBT of that chunk.
    toChunkNbt :: B.ByteString -> NBT
    toChunkNbt bs =
      let (byteCount, compression) = runGet getChunkMeta bs in
      let zChunkData = B.take (fromIntegral byteCount-1) $ B.drop 5 bs in
      decode . decompressWith compression $ zChunkData 
      where
        getChunkMeta = do
          byteCount <- getWord32be
          compressionType <- getWord8
          return (byteCount,compressionType)

        decompressWith compression = case compression of
          1 -> GZip.decompress
          2 -> Zlib.decompress
          _ -> error $ "Unsupported compression type: " ++ show compression
          

-- TODO Modifying block data (especially adding a lot of detail) 
-- can cause an inflation in the size of the chunk data. When stored again
-- in the region file, this may mess up offsets in the header. 
-- The entire region file should be read, understood, before its chunks are
-- modified and the file as a whole re-written out into the file. 
saveRegion :: SavedGameDirectory -> RegionCoords -> NBT -> IO ()
saveRegion dir (x,z) = undefined
