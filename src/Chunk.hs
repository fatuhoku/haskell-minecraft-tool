{-# LANGUAGE TypeSynonymInstances #-}
module Chunk where

{---------}
{- Chunk -}
{---------}

import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zlib as Zlib

import Control.Monad

import Control.Applicative
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as UTF8 ( fromString, toString )
import Data.Array hiding (indices)
import Data.Binary
import Data.List
import Data.Maybe
import Data.NBT

import Block
import Coords
import Types
import Utils

{- CHUNK SERIALIZATION -}

-- Fully represents the "Blocks" TAG_Byte_Array tag of the ChunkNBT.
data BlockIds = BlockIds (Array CellCoords BlockId)
  deriving (Eq, Show)

-- Fully represents the "Data" TAG_Byte_Array tag of the ChunkNBT.
data BlockData = BlockData (Array CellCoords BlockDatum)
  deriving (Eq,Show)

-- instance Show BlockData where
--   show (BlockData bd) = "BlockData: ["++show (bounds bd)++"]"


indices = [(x,z,y) | x <- [0..chunkSizeX-1],
                     z <- [0..chunkSizeZ-1],
                     y <- [0..chunkSizeY-1]]

-- We will just use BlockId as a type synonym right now. 
-- I would imagine it's very similar for the data array as well. Maybe we can
-- abstract out something here.
instance Binary BlockIds where
  get = do
    bytes <- replicateM numCellsInChunk getWord8
    return . BlockIds $ array (arrMin,arrMax) $ zip indices bytes
    where
      arrMin = (0,0,0)
      arrMax = (chunkSizeX-1,chunkSizeZ-1,chunkSizeY-1)

  put (BlockIds bids) = forM_ ((bids !) <$> indices) putWord8

-- We only have half the data...
instance Binary BlockData where
  get = do
    bytes <- replicateM (numCellsInChunk `div` 2) getWord8
    let nybbles = concatMap (tupleToList.swap.toNybbles) bytes
    return . BlockData $ array (arrMin, arrMax) $ zip indices nybbles
    where
      arrMin = (0,0,0)
      arrMax = (chunkSizeX-1,chunkSizeZ-1,chunkSizeY-1)
      swap (a,b) = (b,a)
 
  -- bds is an array of nybbles)...
  put (BlockData bds) = mapM_ (putWord8.fromNybbles) $ zip msn lsn
    where
      nybbles = (bds !) <$> indices
      -- Most and least significant nybbles
      -- The list goes [least,most,least,most]...
      lsn = map fst $ filter snd $ zip nybbles $ cycle [True,False]
      msn = map fst $ filter snd $ zip nybbles $ cycle [False,True]


{- CHUNK EDIT FUNCTIONS -}
-- Does not update timestamps! Uses the same compression method!
-- Lifts a function on NBTs to a function on CompressedChunks
-- by wrapping the appropriate compression and decompression around the sides.
liftCc :: (NBT -> NBT) -> CompressedChunk -> CompressedChunk
liftCc f cc@(CompressedChunk chunkData format ts) =
  mapCc (compressWith format.encode.f.decode.decompressWith format) cc
  where 
    -- A bit like a functor instance. Lifts NBTs to the level of CompressedChunks
    mapCc :: (B.ByteString -> B.ByteString) -> CompressedChunk -> CompressedChunk
    mapCc g cc@(CompressedChunk cnbt _ _) = cc {compressedChunkNbt=g cnbt}

-- Decompresses an NBT from a compressed chunk, discarding the timestamp
chunkFromCc :: CompressedChunk -> Chunk
chunkFromCc cc@(CompressedChunk chunkData format ts) =
  decode $ decompressWith format chunkData

decompressWith :: CompressionFormat -> B.ByteString -> B.ByteString
decompressWith format = case format of
  GZip -> GZip.decompress
  Zlib -> Zlib.decompress

compressWith :: CompressionFormat -> B.ByteString -> B.ByteString
compressWith format = case format of
  GZip -> GZip.compress
  Zlib -> Zlib.compress

-- Convert from cell coordinates to chunk coordinates
toChunkCoords :: CellCoords -> ChunkCoords
toChunkCoords (x,z,_) = (cellToChunk x, cellToChunk z)
  where
    cellToChunk n = floor (fromIntegral n/16.0)

-- The following formula is not required because interpretation of the
-- underlying array is done via array indices.
-- Find index of byte in bytestring that represents given cell coordinate.
-- toChunkIndex :: CellCoords -> ChunkIndex
-- toChunkIndex (x,z,y) = y + z * chunkSizeY + x * chunkSizeY * chunkSizeZ
