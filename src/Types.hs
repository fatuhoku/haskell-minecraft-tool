{-# LANGUAGE TemplateHaskell #-}
module Types where

{---------}
{- Types -}
{---------}
-- A module that contains type definitions.

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as UTF8 ( fromString, toString )
import qualified Text.Show.Pretty as Pr
import Data.Array
import Data.Binary -- ( Binary (..), decode, encode )
import Data.Binary.Get
import Data.List
import Data.Maybe
import Data.NBT
import Text.Printf
import Control.Applicative
import Control.Monad
import System.Directory
import System.IO
import Coords
import Compression

type Timestamp = Word32
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
