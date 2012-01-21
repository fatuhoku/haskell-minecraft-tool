{-# LANGUAGE TemplateHaskell #-}
module Types where

{---------}
{- Types -}
{---------}
-- A module that contains type definitions.

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as UTF8 ( fromString, toString )
import Data.Array
import Data.Binary -- ( Binary (..), decode, encode )
import Data.Binary.Get
import Data.List
import Data.Maybe
import Data.NBT

import qualified Text.Show.Pretty as Pr
import Text.Printf

import Control.Applicative
import Control.Monad

import System.Directory
import System.IO

import Coords

-- Represents the contents of a region file.
--   A region is an array of possible chunks;
--   Nothing are chunks that have not been generated yet.
--   Just chunks are those that have relevant data in the file.
--   The data is kept uncompressed unless it is needed.
data Region = Region (Array (X,Z) (Maybe CompressedChunk))
  deriving (Eq,Show)

data CompressedChunk = CompressedChunk {
  compressedChunkNbt :: B.ByteString,
  compressedChunkFormat :: CompressionFormat,
  compressedChunkTimestamp :: Timestamp }
--  deriving (Eq,Show)
  deriving (Eq)
instance Show CompressedChunk where
  show (CompressedChunk {compressedChunkNbt=nbt,compressedChunkFormat=format}) =
    "l: "++ show (B.length nbt)

data CompressionFormat = GZip | Zlib deriving (Eq, Show)

type Timestamp = Word32

-- The chunks are simply 
data Chunk = Chunk Timestamp NBT
type Byte = Word8
type Nybble = Word8 -- Waps a Nybble

-- A pair of bytestrings
--  - the array of BlockIDs (1 byte per block)
--  - the array of DataIDs  (1 nybble per block)
data ChunkData = ChunkData {
  blockIds :: B.ByteString,
  dataIds :: B.ByteString    -- NybbleString really.
  }

type DataType = String

----------------------------
-- SYB TH Derivations
----------------------------
-- The most important is deriving a Data instance for NBT such that
-- scrap your zippers can work on it.
-- $(derive makeData ''NBT)
-- $(derive makeTypeable ''NBT)
-- $(derive makeData ''TagType)
-- $(derive makeTypeable ''TagType)
-- 
-- $(derive makeData ''Region)
-- $(derive makeTypeable ''Region)
-- $(derive makeData ''CompressedChunk)
-- $(derive makeTypeable ''CompressedChunk)
-- -- $(derive makeData ''Chunk)
-- -- $(derive makeTypeable ''Chunk)
-- $(derive makeData ''CompressionFormat)
-- $(derive makeTypeable ''CompressionFormat)
