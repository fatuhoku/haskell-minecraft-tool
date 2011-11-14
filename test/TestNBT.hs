{-# LANGUAGE TemplateHaskell #-}
module TestNBT where

import qualified Data.ByteString.Lazy as B
import Data.NBT
import Data.Generics.Zipper
import Data.Data
import Data.DeriveTH
import Data.Typeable
import Data.Derive.Data
import Data.Derive.Typeable
import Data.Monoid

nbt1 = CompoundTag (Just "Level") [
  DoubleTag (Just "Zero") 0.0,
  ByteArrayTag (Just "Blocks") 2 (2 `B.cons` (1 `B.cons` B.empty)),
  IntTag (Just "One") 1,
  ByteArrayTag (Just "Data") 1 (1 `B.cons` B.empty)
  ]

value = undefined
-- let g1 = toZipper nbt1 in
-- let Just g2 = down g1 in getHole g2 
--        let Just g3 = getHole g2 :: Maybe [NBT]


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
