module Level where

import Codec.Compression.GZip as GZip
import Codec.Compression.Zlib as Zlib
import Control.Applicative
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
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

-- This data structure presents the data contained within the Level.dat NBT
-- TODO Expand on this definition to parse the data out of the NBT.
newtype Level = Level NBT deriving (Show, Eq)

-- TODO Proper Binary instance of Level.
-- This is simply provided by NBT at the moment.
-- For use of encode and decode in FileIO.
-- instance Binary Level where
--   get = decode
--   put = encode
--
instance Binary Level where
  get = do
    bs <- getRemainingLazyByteString
    return . Level $ runGet (get::Get NBT) (GZip.decompress bs)
  put (Level nbt) = do
    putLazyByteString $ GZip.compress $ runPut (put nbt) 
    
-----------------------------------------------------------------------
-- This is the future Level data structure.
-----------------------------------------------------------------------
-- data Level = {
--   Data :: TAG_Compound, -- World data.
--   Time :: TAG_Long, -- Stores the current "time of day" in ticks. There are 20 ticks per real-life second, and 24000 ticks per Minecraft day/night cycle, making the full cycle length 20 minutes. 0 is the start of daytime, 12000 is the start of sunset, 13800 is the start of nighttime, 22200 is the start of sunrise, and 24000 is daytime again. The value stored in level.dat is always increasing and can be larger than 24000, but the "time of day" is always modulo 24000 of the "Time" field value.
--   LastPlayed :: TAG_Long, -- Stores the Unix time stamp (in milliseconds) when the player saved the game.
--   Player :: TAG_Compound, -- Player entity information. See Entity Format and Mob Entity Format for details. Has additional elements, --
--   Inventory :: TAG_List, -- Each TAG_Compound in this list defines an item the player is carrying, holding, or wearing as armor.
--   TAG_Compound, -- Inventory item data
  --   id :: TAG_Short, -- Item or Block ID.
  --   Damage :: TAG_Short, -- The amount of wear each item has suffered. The maximum durability of the tool (for example, 33 for golden tools) means undamaged. When the Damage reaches 0, it breaks and disappears. Only tools and armor accumulate damage normally.
  --   Count :: TAG_Byte, -- Number of items stacked in this inventory slot. Any item can be stacked, including tools, armor, and vehicles. Range is 1-255. Values above 127 are not displayed in-game.
  --   Slot :: TAG_Byte, -- Indicates which inventory slot this item is in.
--   abilities :: TAG_Compound, -- Determines the player's capabilities. Added in Beta 1.9-Pre5
--   flying :: TAG_Byte, -- Supposed to be set when the player is flying. Appears to be ignored by the game upon loading anyway. Set to 1 on creative mode, 0 on survival. Added in Beta 1.9-Pre5
--   instabuild :: TAG_Byte, -- No effect? Set to 1 on creative mode, 0 on survival. Added in Beta 1.9-Pre5
--   mayfly :: TAG_Byte, -- Determines whether the player can fly. Set to 1 on creative mode, 0 on survival. Added in Beta 1.9-Pre5
--   invulnerable :: TAG_Byte, -- Whether the player is invulnerable. Set to 1 on creative mode, 0 on survival. Added in Beta 1.9-Pre5
--   Score :: TAG_Int, -- Current score, doesn't appear to be implemented yet. Always 0. This tag exists only in SSP.
--   Dimension :: TAG_Int, -- Which dimension the player is in. 0 is the Overworld, -1 is the Nether, and 1 is the End.
--   SpawnX :: TAG_Int, -- X coordinate of the player's spawn position. Default is 0.
--   SpawnY :: TAG_Int, -- Y coordinate of the player's spawn position. Default is 64.
--   SpawnZ :: TAG_Int, -- Z coordinate of the player's spawn position. Default is 0.
--   SizeOnDisk :: TAG_Long, -- Estimated size of the entire world in bytes.
--   RandomSeed :: TAG_Long, -- Random number providing the Random Seed for the terrain.
--   version :: TAG_Int, -- Current version of NBT. When announced, -- 19132. Added in version Beta 1.3.
--   LevelName :: TAG_String, -- Specifies the name of the level. Added in version Beta 1.3.
--   raining :: TAG_Byte, -- 1 or 0(true/false). Raining. Added in version Beta 1.5.
--   thundering :: TAG_Byte, -- 1 or 0(true/false). Thundering. Can be set to 1 while "raining" is 0, in which case it appears to have no effect on the game. Added in version Beta 1.5.
--   rainTime :: TAG_Int, -- Countdown timer (in ticks) until the next true/false change of the "raining" field. When the timer reaches 0, the "raining" field toggles, and the "rainTime" countdown is reset to a new (presumably random) value. Added in version Beta 1.5.
--   thunderTime :: TAG_Int, -- Similar countdown timer for the next true/false "thundering" field change. Added in version Beta 1.5.
--   GameType :: TAG_Int, -- Whether in survival  0 or in creative  1 mode. Added in version Beta 1.8 pre-release 1.
--   MapFeatures :: TAG_Byte, -- Whether structures (dungeons, mob villages, etc.) will be generated. Added in version Beta 1.8 pre-release 1.
--   hardcore :: TAG_Byte
-- }-- Whether the map should be locked in hardcore mode. This can be set to 0 or 1 to toggle the state of a map, even after it is created.
-----------------------------------------------------------------------



-- TODO Differentiate between level.dat NBT versions and pick the apporpriate
-- implementation to extract the player's position correctly.

-- This is the old version of getting the player coords. 
getPlayerCoords' :: NBT -> PlayerCoords
getPlayerCoords' (CompoundTag (Just "" ) tags) =
  let [(CompoundTag (Just "Data") dtags)] = tags in
  case findPlayerTag dtags of
    Nothing -> error "Player tag not found."
    Just ptag -> let ptagContents = compoundContents ptag in
      let [x,y,z] = map getInt $ filter (isPrefixOf "Spawn" . fromJust . getName)
                      $ filter (isJust . getName) ptagContents
      in PlayerCoords (x,y,z)
  where
    findPlayerTag = find isPlayerTag
    isPlayerTag (CompoundTag (Just "Player") _) = True
    isPlayerTag _ = False
getPlayerCoords' _ = error "Invalid level.dat NBT: does not begin with Data CompoundTag"

-- Total function that extracts the player's position out of a level.dat NBT.
getPlayerCoords :: NBT -> Maybe PlayerCoords
getPlayerCoords nbt = do
  posList <- getData =<< moveToTag "Pos" (toZipper nbt) :: Maybe [NBT]
  let [x,y,z] = catMaybes $ map (getData.toZipper) posList :: [Double]
  let [x',y',z'] = map truncate [x,y,z]
  Just $ PlayerCoords (x',z',y')
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

