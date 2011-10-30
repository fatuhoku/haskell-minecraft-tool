module Chunk where

import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zlib as Zlib

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as UTF8 ( fromString, toString )

import Types
import Data.Binary

-- | Compress the chunk back into a compressed chunk
compressChunk :: CompressionFormat -> Timestamp -> Chunk -> CompressedChunk
compressChunk format ts chunk =
  CompressedChunk (compressWith format $ encode chunk) format ts

-- | Decompress the chunk as Chunk (NBT). Loses timestamp information.
decompressChunk :: CompressedChunk -> Chunk
decompressChunk (CompressedChunk chunkData format ts) =
  decode . decompressWith format $ chunkData 

-- Give a way to access the contents of the compressed chunk as a NBT object
-- Does not update the timestamp
withChunk :: CompressedChunk -> (Chunk -> Chunk) -> CompressedChunk
withChunk cc@(CompressedChunk chunkData format ts) transformer =
  compressChunk format ts $ transformer $ decompressChunk cc

decompressWith :: CompressionFormat -> B.ByteString -> B.ByteString
decompressWith format = case format of
  GZip -> GZip.decompress
  Zlib -> Zlib.decompress

compressWith :: CompressionFormat -> B.ByteString -> B.ByteString
compressWith format = case format of
  GZip -> GZip.compress
  Zlib -> Zlib.compress
