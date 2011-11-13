module Types where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as UTF8 ( fromString, toString )
import Data.Binary -- ( Binary (..), decode, encode )
import Data.Binary.Get
import Data.List
import Data.Maybe
import Data.NBT
import Data.Array

import qualified Text.Show.Pretty as Pr
import Text.Printf

import Control.Applicative
import Control.Monad

import System.Directory
import System.IO


-- Represents the contents of a region file.
--   A region is an array of possible chunks;
--   Nothing are chunks that have not been generated yet.
--   Just chunks are those that have relevant data in the file.
--   The data is kept uncompressed unless it is needed.
data Region = Region (Array (X,Z) (Maybe CompressedChunk))
--  deriving (Eq,Show)
  deriving (Eq)
instance Show Region where
  show (Region reg) = show $ elems reg

data CompressedChunk = CompressedChunk {
  compressedChunkNbt :: B.ByteString,
  compressedChunkFormat :: CompressionFormat,
  compressedChunkTimestamp :: Timestamp }
--  deriving (Eq,Show)
  deriving (Eq)
instance Show CompressedChunk where
  show (CompressedChunk {compressedChunkNbt=nbt,compressedChunkFormat=format}) =
    "l: "++ show (B.length nbt) ++ " f: " ++ show format

data CompressionFormat = GZip | Zlib deriving (Eq, Show)

type X = Int -- X coordinate type
type Z = Int -- Z coordinate type
type Timestamp = Word32
type RegionCoords = (Int, Int)
type ChunkCoords = (Int, Int)
type CellCoords = (Int, Int, Int)
type WorldDirectory = FilePath

type Chunk = NBT
type ChunkIndex = Int

-- A pair of bytestrings
--  - the array of BlockIDs (1 byte per block)
--  - the array of DataIDs  (1 nibble per block)
data ChunkData = ChunkData {
  blockIds :: B.ByteString,
  dataIds :: B.ByteString
  }

type DataType = String


-- A cell replacement represents the change of blocktype at some position
-- in the game world to to the specified BlockType.
data CellReplacement = CR {
  cell ::  CellCoords,
  blockId :: BlockType,
  dataId  :: DataType
  }

data BlockType = Wool

instance Binary BlockType where
  put Wool = put (35 :: Word32)
  get = do
    w <- get :: Get Word32
    return $ case w of
      35 -> Wool
      _  -> error $ "Unsupported block type: " ++ show w

data WoolColour = White
                | Orange
                | Magenta
                | LightBlue
                | Yellow
                | Lime
                | Pink
                | Gray
                | LightGray
                | Cyan
                | Purple
                | Blue
                | Brown
                | Green
                | Red
                | Black
                deriving (Show, Eq)

instance Binary WoolColour where
  get = do
    w <- get :: Get Word16
    return $ case w of 
      0  -> White
      1	 -> Orange
      2	 -> Magenta
      3	 -> LightBlue
      4	 -> Yellow
      5	 -> Lime
      6	 -> Pink
      7	 -> Gray
      8	 -> LightGray
      9	 -> Cyan
      10 -> Purple    	
      11 -> Blue      	
      12 -> Brown     	
      13 -> Green     	
      14 -> Red       	
      15 -> Black     	
      x  -> error $ "Unknown wool colour: " ++ show x

  put White     = put (0 :: Word16)
  put Orange    = put (1 :: Word16)
  put Magenta   = put (2 :: Word16)
  put LightBlue = put (3 :: Word16)
  put Yellow    = put (4 :: Word16)
  put Lime      = put (5 :: Word16)
  put Pink      = put (6 :: Word16)
  put Gray      = put (7 :: Word16)
  put LightGray = put (8 :: Word16)
  put Cyan      = put (9 :: Word16)
  put Purple    = put (10:: Word16)
  put Blue      = put (11:: Word16)
  put Brown     = put (12:: Word16)
  put Green     = put (13:: Word16)
  put Red       = put (14:: Word16)
  put Black     = put (15:: Word16)

type WoolColourArray = Array (Int,Int) WoolColour
