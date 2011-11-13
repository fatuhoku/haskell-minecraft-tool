module NBTExtras where

import Data.NBT
import Text.Printf


-- This file could be home to some of the generic zipper helpers.

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

compoundContents (CompoundTag _ contents) = contents
compoundContents _ = error "Not CompoundTag"
