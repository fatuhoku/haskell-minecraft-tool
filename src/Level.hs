module Level where

import Codec.Compression.GZip as GZip
import Codec.Compression.Zlib as Zlib
import Control.Applicative
import Data.Binary
import Data.List
import Data.Maybe
import Data.NBT
import Data.Generics
import Data.Generics.Zipper
import qualified Data.ByteString.Lazy as B

import NBTExtras
import Types
import Coords
import Access

-- Loads a level.dat file given the saved game directory.
loadLevel :: WorldDirectory -> IO NBT
loadLevel dir = do
  let fn = dir ++ "/level.dat"
  fileL <- GZip.decompress <$> B.readFile fn
  let file = B.pack (B.unpack fileL)
      dec = (decode file :: NBT)
      enc = encode dec
  return dec

-- TODO Differentiate between level.dat NBT versions and pick the apporpriate
-- implementation to extract the player's position correctly.

-- This is the old version of getting the player coords. 
getPlayerCoords' :: NBT -> CellCoords
getPlayerCoords' (CompoundTag (Just "" ) tags) =
  let [(CompoundTag (Just "Data") dtags)] = tags in
  case findPlayerTag dtags of
    Nothing -> error "Player tag not found."
    Just ptag -> let ptagContents = compoundContents ptag in
      let [x,y,z] = map getInt $ filter (isPrefixOf "Spawn" . fromJust . getName)
                      $ filter (isJust . getName) ptagContents
      in (x,y,z)
  where
    findPlayerTag = find isPlayerTag
    isPlayerTag (CompoundTag (Just "Player") _) = True
    isPlayerTag _ = False
getPlayerCoords' _ = error "Invalid level.dat NBT: does not begin with Data CompoundTag"

-- Total function that extracts the player's position out of a level.dat NBT.
getPlayerCoords :: NBT -> Maybe CellCoords
getPlayerCoords nbt = do
  posList <- getData =<< moveToTag "Pos" (toZipper nbt) :: Maybe [NBT]
  let [x,y,z] = catMaybes $ map (getData.toZipper) posList :: [Double]
  let [x',y',z'] = map truncate [x,y,z]
  return (x',z',y')
  where
    getList (ListTag _ _ _ list) = list

    -- The rightmost (down) element of a zipper is the relevant data:
    -- ByteTag -> Int8
    -- ShortTag -> Int16
    -- IntTag (Maybe String) Int32	 
    -- LongTag -> Int64	 
    -- FloatTag -> Float	 
    -- DoubleTag -> Double	 
    -- ByteArrayTag -> ByteString	 
    -- StringTag -> String	 
    -- ListTag -> [NBT]	 
    -- CompoundTag -> [NBT]
    getData nbt = down nbt >>= getHole
-- getPlayerCoords _ = error "Invalid level.dat NBT: does not begin with Data CompoundTag"

