module Block where

import Data.Array
import Data.Binary
import Types

{- This module contains block data and block ids
 - Most blocks in the Minecraft world can fully (??) be identified and defined
 - by one blockId (Word8) and one blockData(Word4).
 - blockId defines the type of block (wood, sand, wheat...) whereas
 - blockData defines a further detail about this kind of block
 - (wood: ash, birch, oak)
 - (wheat: growth levels [1..9])
 -
 - ChunkData can really just be interpreted as a bunch of cells, enumerated over
 - in a particular order.
 - - Read Word8's from both bytestrings, and zip them together.
 -
 - Strongly typed blocks aren't really necessary; they just amount to lots of
 - typing for little gain.
 - wool == 35
 - red = ...
 - -}

type BlockId = Byte
type BlockDatum = Nybble -- Wraps a Word4
data Block = Block BlockId BlockDatum

-- A cell replacement represents the change of blocktype at some position
-- in the game world to to the specified BlockType.
-- data CellReplacement = CR {
--   cell ::  CellCoords,
--   blockId :: BlockType,
--   dataId  :: DataType
--   }

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

