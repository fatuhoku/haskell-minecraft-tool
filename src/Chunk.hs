{-# LANGUAGE TypeSynonymInstances #-}
module Chunk where

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

data BlockData = BlockData (Array CellCoords BlockDatum)
  deriving (Eq, Show)

indices = [(x,z,y) | x <- [0..chunkSizeX-1],
                     z <- [0..chunkSizeZ-1],
                     y <- [0..chunkSizeY-1]]

-- We will just use BlockId as a type synonym right now. 
-- I would imagine it's very similar for the data array as well. Maybe we can
-- abstract out something here.
instance Binary BlockIds where
  get = do
    bytes <- replicateM numCellsInChunk getWord8
    return . BlockIds $ array (arrayMin,arrayMax) $ zip indices bytes
    where
      arrayMin = (0,0,0)
      arrayMax = (chunkSizeX-1,chunkSizeZ-1,chunkSizeY-1)

  put (BlockIds bids) = forM_ ((bids !) <$> indices) putWord8

instance Binary BlockData where
  get = do
    bytes <- replicateM numCellsInChunk getWord8
    let nybbles = concatMap (tupleToList.toNybbles) bytes
    return . BlockData $ array (arrayMin, arrayMax) $ zip indices nybbles
    where
      arrayMin = (0,0,0)
      arrayMax = (chunkSizeX-1,chunkSizeZ-1,chunkSizeY-1)
 
  -- bds is an array of nybbles)...
  put (BlockData bds) = mapM_ (putWord8.fromNybbles) $ zip odds evens
    where
      nybbles = (bds !) <$> indices
      odds = map fst $ filter snd $ zip nybbles $ cycle [True,False]
      evens = map fst $ filter snd $ zip nybbles $ cycle [False,True]


{- CHUNK EDIT FUNCTIONS -}
-- Does not update timestamps! Uses the same compression method!
-- Lifts a NBT homomorphism into CompressedChunk homomorphism
-- TODO abstract: For any Binary value, we can derive ByteString -> ByteString 
-- homomorphism.
modifyCc :: (NBT -> NBT) -> CompressedChunk -> CompressedChunk
modifyCc f cc@(CompressedChunk chunkData format ts) =
  mapCc (compressWith format.encode.f.decode.decompressWith format) cc
  where 
    -- A bit like a functor instance. Lifts NBTs to the level of CompressedChunks
    mapCc :: (B.ByteString -> B.ByteString) -> CompressedChunk -> CompressedChunk
    mapCc g cc@(CompressedChunk cnbt _ _) = cc {compressedChunkNbt=g cnbt}

decompressWith :: CompressionFormat -> B.ByteString -> B.ByteString
decompressWith format = case format of
  GZip -> GZip.decompress
  Zlib -> Zlib.decompress

compressWith :: CompressionFormat -> B.ByteString -> B.ByteString
compressWith format = case format of
  GZip -> GZip.compress
  Zlib -> Zlib.compress

-- Given a region NBT, we want to produce a 3D array, holding all that lovely data.
-- The byteArray is actually rather nicely given as a ByteString.
-- We just need a way of converting that bytestring into 3D array.
-- Extracting the Blocks tag shouldn't be too difficult.
--
-- We can access a block's location by the 
-- unsigned char BlockID = Blocks[ y + z * ChunkSizeY(=128) + x * ChunkSizeY(=128) * ChunkSizeZ(=16) ];
-- loadChunk :: NBT -> B.ByteString
-- loadChunk (CompoundTag (Just "Level") ltags) =
--   let (ByteArrayTag _ bsLength blocks) = fromJust $ maybeBlocksTag
--   in blocks
--   where
--     maybeBlocksTag = find (("Blocks" ==) . fromJust . getName) ltags

-- TODO Write the function that would apply a cell replacement to the
-- chunk data bytestrings.
-- applyReplacement :: CellReplacement -> ChunkData -> ChunkData
-- applyReplacement (CR (x,z,y) id did) cd = error "applyReplacement: not implemented"

-- Convert from cell coordinates to chunk coordinates
toChunkCoords :: CellCoords -> ChunkCoords
toChunkCoords (x,z,_) = (cellToChunk x, cellToChunk z)
  where
    cellToChunk n = floor (fromIntegral n/16.0)

-- Find index of byte in bytestring that represents given cell coordinate.
toChunkIndex :: CellCoords -> ChunkIndex
toChunkIndex (x,z,y) = y + z * chunkSizeY + x * chunkSizeY * chunkSizeZ
