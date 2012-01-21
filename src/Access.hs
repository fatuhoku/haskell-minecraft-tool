{-# LANGUAGE TemplateHaskell #-}
module Access where

import Data.Array
import Data.Binary
import Data.Data
import Data.Derive.Data
import Data.Derive.Typeable
import Data.DeriveTH
import Data.Generics
import Data.Generics.Zipper
import Data.Monoid
import Data.NBT
import Data.Typeable
import qualified Data.ByteString.Lazy as B

import Block
import Coords
import Chunk
import Types
import Utils
import Control.Applicative

-- value = undefined
-- let g1 = toZipper nbt1 in
-- let Just g2 = down g1 in getHole g2 
--        let Just g3 = getHole g2 :: Maybe [NBT]

-- We would like to replace a part of the NBT: strictly speaking, the 
-- zeverywhere would help me navigate to the right NBT 
--
-- 1) Find the Blocks array and update it.
-- 2) Find the Data array and update it.
--
-- For some reason, the series of updates couldn't be performed all at the same
-- time. I'm not sure why...
--
-- We will just leave all the timestamps the same as before, for simplicity.
-- We accept a number of transformations that would modify the NBT in many
-- ways...
-- TODO move foldr1 (.) fs into its own function 'compose'
updateChunk :: [NBT -> NBT] -> Chunk -> Chunk
updateChunk [] chunk = chunk
updateChunk (f:fs) chunk = updateChunk fs (everywhere (mkT f) chunk) -- . vtrace "Updating NBT: " 

-------------------------------------------------
-- START FUNCTIONS
-------------------------------------------------

-- Transition function that applies to an NBT I'm interested in.
-- These are kind of expensive :/
setBlockIds :: B.ByteString -> NBT -> NBT
setBlockIds bs (ByteArrayTag (Just "Blocks") len _) =
  ByteArrayTag (Just "Blocks") (fromIntegral $ B.length bs) bs
setBlockIds _ x = x

setBlockData :: B.ByteString -> NBT -> NBT
setBlockData bs (ByteArrayTag (Just "Data") len _) =
  ByteArrayTag (Just "Data") (fromIntegral $ B.length bs) bs
setBlockData _ x = x

-- BuildBlock 
blockIdUpdates :: [(LocalCoords, BlockId)] ->  NBT -> NBT
blockIdUpdates updates (ByteArrayTag (Just "Blocks") _ bs) = 
  ByteArrayTag (Just "Blocks") (fromIntegral numCellsInChunk) bs'
  where
    (BlockIds arr) = decode bs :: BlockIds
    bs' = encode $ BlockIds (arr // updates)
blockIdUpdates _ nbt = nbt

blockDataUpdates :: [(LocalCoords, BlockDatum)] ->  NBT -> NBT
blockDataUpdates updates (ByteArrayTag (Just "Data") _ bs) = 
  ByteArrayTag (Just "Data") (fromIntegral numCellsInChunk `div` 2) bs'
  where
    (BlockData arr) = decode bs :: BlockData
    bs' = encode $ BlockData (arr // updates)
blockDataUpdates _ nbt = nbt

-------------------------------------------------
-- END FUNCTIONS
-------------------------------------------------
    
-- If the hole matches then return the z.
-- 'plz' means 'possibleListOfZippers'. This is a [NBT] arising from the last
-- part of either the ListTag or the CompoundTag.
moveToTag :: String -> Zipper NBT -> Maybe (Zipper NBT)
moveToTag name z = do
  z2 <- down' z
  -- This condition fails when no name available, Nothing name, or non-matching name
  if getHole z2 == Just (Just name)
    then Just z
    else do
      plz <- down z
      _ <- getHole plz :: Maybe [NBT] -- ensure this cast to [NBT] is valid
      moveToTagList name plz

-- Continue the threading through multiple items in a list.
-- IF we're looking at a cons, we ensure that the zipper is executed
-- for every one in the list.
-- This is the zipper analogous to 'find'. Look at head by navigation, if , and 
moveToTagList :: String -> Zipper NBT -> Maybe (Zipper NBT)
moveToTagList name z = do
  left <- down' z -- Left child; the 'a' of (a:as)
  right <- down z  -- Right child; the 'as' of (a:as)
  getFirst . mconcat $
      map First [moveToTag name left, moveToTagList name right]
                    
$(derive makeData ''NBT)
$(derive makeTypeable ''NBT)
$(derive makeData ''Chunk)
$(derive makeTypeable ''Chunk)
$(derive makeData ''TagType)
$(derive makeTypeable ''TagType)
-- 
-- $(derive makeData ''Region)
-- $(derive makeTypeable ''Region)
-- $(derive makeData ''CompressedChunk)
-- $(derive makeTypeable ''CompressedChunk)
-- -- $(derive makeData ''Chunk)
-- -- $(derive makeTypeable ''Chunk)
-- $(derive makeData ''CompressionFormat)
-- $(derive makeTypeable ''CompressionFormat)
