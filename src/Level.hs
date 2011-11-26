module Level where

import Data.NBT
import Data.Binary
import Data.List
import Data.Maybe
import Types
import Codec.Compression.GZip as GZip
import Codec.Compression.Zlib as Zlib
import Control.Applicative
import qualified Data.ByteString.Lazy as B
import NBTExtras

-- Loads a level.dat file given the saved game directory.
loadLevel :: WorldDirectory -> IO NBT
loadLevel dir = do
  let fn = dir ++ "/level.dat"
  fileL <- GZip.decompress <$> B.readFile fn
  let file = B.pack (B.unpack fileL)
      dec = (decode file :: NBT)
      enc = encode dec
  return dec

-- TODO Use a Zipper for the NBT structure instead of a hard-coded path.
getPlayerCoords :: NBT -> CellCoords
getPlayerCoords (CompoundTag (Just "" ) tags) =
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
getPlayerCoords _ = error "Invalid level.dat NBT: does not begin with Data CompoundTag"
