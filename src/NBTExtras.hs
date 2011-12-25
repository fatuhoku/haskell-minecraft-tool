module NBTExtras where

import Data.NBT
import Text.Printf
import Data.Generics.Zipper


-- This file could be home to some of the generic zipper helpers.
-- Get the rightmost (down) element of a zipper is the relevant data:
--   ByteTag -> Int8
--   ShortTag -> Int16
--   IntTag (Maybe String) Int32	 
--   LongTag -> Int64	 
--   FloatTag -> Float	 
--   DoubleTag -> Double	 
--   ByteArrayTag -> ByteString	 
--   StringTag -> String	 
--   ListTag -> [NBT]	 
--   CompoundTag -> [NBT]
getData nbt = down nbt >>= getHole

-- Some standard extractors
getName :: NBT -> Maybe String
getName EndTag = Nothing
getName (ByteTag      name _) = name
getName (ShortTag     name _) = name
getName (IntTag       name _) = name
getName (LongTag      name _) = name
getName (FloatTag     name _) = name
getName (DoubleTag    name _) = name
getName (ByteArrayTag name _ _) = name
getName (StringTag    name _ _) = name
getName (ListTag      name _ _ _) = name
getName (CompoundTag  name _) = name

getInt (IntTag _ n) = fromIntegral n
getInt t = error $ printf "getInt: %s is not an IntTag." (show t)

getList (ListTag _ _ _ list) = list

compoundContents (CompoundTag _ contents) = contents
compoundContents _ = error "Not CompoundTag"
