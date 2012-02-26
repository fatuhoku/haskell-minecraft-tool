{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
module Main where

import Control.Monad
import Data.Array
import Data.Binary
import Data.Generics.Zipper
import Data.Int
import Data.List
import Data.Maybe
import Data.NBT
import Distribution.Simple.Utils
import Distribution.Verbosity
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit.Base hiding (Test)
import Test.QuickCheck
import qualified Data.ByteString.Lazy as B
import qualified Test.HUnit as H
import qualified Codec.Compression.GZip as G
import qualified Codec.Compression.Zlib as Z

import Block
import Chunk
import Compression
import Coords
import FileIO
import Generics
import Level
import NBTExtras
import Region
import Types
import Utils
import World

-- Test Main
-- ============================================================================
main :: IO ()
main = $(defaultMainGenerator)

-- Test Data
-- ============================================================================
nbt1 = CompoundTag (Just "Level")
  [DoubleTag (Just "Zero") 0.0
  ,ByteArrayTag (Just "Blocks") 2 (2 `B.cons` (1 `B.cons` B.empty))
  ,IntTag (Just "One") 1
  ,ByteArrayTag (Just "Data") 1 (1 `B.cons` B.empty)
  ]

-- A simple NBT representing a 16x16x128 block of pure gold.
-- The data field is notably 0.
nbt2 :: NBT 
nbt2 = CompoundTag (Just "Level") [
  chunkOfPureGold,
  dataPureZero
  ]

chunkOfPure :: Word8 -> NBT
chunkOfPure bid = 
  let size = fromIntegral numCellsInChunk
      bs = B.pack (replicate numCellsInChunk bid) in
  ByteArrayTag (Just "Blocks") size bs

-- Interesting; even though I've set all the blocks to pure gold, 9 appears.
chunkOfPureGold :: NBT
chunkOfPureGold = let gold = 41 in chunkOfPure gold

chunkOfPureWhiteWool :: NBT
chunkOfPureWhiteWool = let wool = 35 in chunkOfPure wool

-- This represents white wool, or the default data value for any block.
-- Create from numCellsInChunk nybbles of 0.
-- Encoding puts it back into a bytestring.
dataPureZero = let zeros = B.pack $ replicate size 0
                   size  = numCellsInChunk `div` 2
                   size' = fromIntegral size :: Int32 in
  ByteArrayTag (Just "Data") size' zeros

-- HUnit test cases
-- ============================================================================
case_MoveToBlockIdWorks =
  Just chunkOfPureGold @=? getHole (fromJust $ moveToTag "Blocks" (toZipper nbt2))
case_MoveToDataTagWorks =
  Just dataPureZero @=? getHole (fromJust $ moveToTag "Data" (toZipper nbt2))
 
-- Put a white wool block in the gold block.
-- This only requires manipulation of the block data array.
case_UpdateChunkUpdatesBlockIds =
  assertEqual (arrayDifferences 20 expectedArr actualArr) expected actual
  where
    -- Get the blocks array of nbt2; expect the same array with one value
    -- changed at locC

    (BlockIds ids) = decode $ getBlockIds nbt2 :: BlockIds
    expected@(BlockIds expectedArr) = BlockIds $ ids // [(locC,wool)]
    actual@(BlockIds actualArr) = decode $ getBlockIds nbt2' :: BlockIds
    nbt2' = updateChunk [updateBlocks] nbt2
    updateBlocks = blockIdUpdates [(locC,wool)]
    locC = (1,1,1)
    wool = 35

-- This test reads the BlockData from nbt2, produces nbt2' by updating its Data
-- then compares the expected array with the (nbt2+delta) updatedData...
case_UpdateChunkUpdatesBlockData =
  assertEqual (arrayDifferences 20 expectedArr actualArr) expected actual
  where
    expected@(BlockData expectedArr) = BlockData $ d // [(locC,colour)]
    actual@(BlockData actualArr) = decode $ getBlockData nbt2' :: BlockData
    (BlockData d) = decode $ getBlockData nbt2 :: BlockData
    nbt2' = updateChunk [blockDataUpdates [(locC,colour)]] nbt2
    locC = (1,1,1)
    colour = 1

-- Copy the world into a new location (testworld_copy)
-- Run the putting of a block of wool five blocks above the player's head
-- Read the file again, and ensure that the black wool block is found, as
-- expected, on top of the player's head.
case_OneBlockPlacesCorrectlyColouredWoolInWorld = do
  worldCopy <- makeCopy "_copy" "worlds/testworld"
  let testId = 35    -- wool
  let testDatum = 15 -- black
  let testBlock = Block testId testDatum
  woolCoord <- putOneBlock worldCopy testBlock
  assertBlock worldCopy woolCoord testBlock


-- TODO Check for correct endianness, arising from the the ambiguity of the
-- following detail from the wiki:
--   "The Data, BlockLight, and SkyLight arrays have four bits for each byte of the
--   Blocks array. The least significant bits of the first byte of the Data,
--   BlockLight, or SkyLight arrays correspond to the first byte of the Blocks
--   array."
-- case_DecodeEncodeUsesCorrectNybbleEndianness

-- QuickCheck2 test properties
-- ============================================================================
-- TODO   There must be a way to set the number of times a property is checked.
prop_DecodeEncodeRegion r = 
  let r' = (decode.encode) r in printTestCase (regionDifferences 10 r r') (r==r')

prop_DecodeEncodeBlockIds :: BlockIds -> Property
prop_DecodeEncodeBlockIds bs =
  let bs' = (decode.encode) bs in printTestCase (blockIdsDifferences 10 bs bs') (bs==bs')

prop_DecodeEncodeBlockData :: BlockData -> Property
prop_DecodeEncodeBlockData bs =
  let bs' = (decode.encode) bs in printTestCase (blockDataDifferences 10 bs bs') (bs==bs')

prop_ToFromNybbles byte = (fromNybbles.toNybbles) byte == byte

-- TODO    Putting a block in a region will change it.
-- Not just any arbitrary CellCoords will do here it must be Bounded.
-- prop_PutBlockRegion :: Region -> CellCoords -> Block -> Bool
-- prop_PutBlockRegion r@(Region reg) coords block = undefined

-- Test utility and helper functions
-- ============================================================================

-- Return the element at the minimum index of both arrays, assuming they have
-- exactly the same bounds
atMin a b
  | bounds a == bounds b = let (i,_) = bounds a in ((i,a!i), (i,b!i))
  | otherwise = error "The arrays' dimensions do not agree"
    
-- Finds the first n different elements between two arrays.
diff n a b = take n $ filter (\(a,b) -> snd a /= snd b) $ zip (assocs a) (assocs b)

-- Convenience functions to use diff with domain data types
diffRegion n (Region a) (Region b) = diff n a b
diffBlockData n (BlockData a) (BlockData b) = diff n a b
diffBlockIds n (BlockIds a) (BlockIds b) = diff n a b

-- A series of debug messages for diffs...
regionDifferences n r r' = unlines
  ["Region data differ from expected."
  ,show (diffRegion 10 r r')
  ]

blockDataDifferences n bds bds' = unlines
  ["Block data differ from expected... "
  ,show (diffBlockData n bds bds')
  ]

blockIdsDifferences n bds bds' = unlines
  ["Block IDS differ from expected... "
  ,show (diffBlockIds n bds bds')
  ]

arrayDifferences n a a' = unlines
  ["Showing the first "++ show n ++"differences."
  ,show (diff n a a')
  ,show (atMin a a')
  ]


-- Decompresses an NBT from a compressed chunk, discarding the timestamp
decompressChunk :: Compressed Chunk -> Chunk
decompressChunk cc@(CompressedWith fmt bs) = decode $ case fmt of
  GZip -> G.decompress bs
  Zlib -> Z.decompress bs

-- Asserts that a world has a particular block at a particular place.
assertBlock :: WorldDirectory -> CellCoords ->  Block -> IO ()
assertBlock dir cell block = do
  let (r,c,l) = toHierarchicalCoords cell
  (Region region) <- loadRegion dir r
  case region ! c of
    Nothing -> fail $ "No such region: " ++ show r
    Just cc -> do
      let chunk = decompressChunk cc :: Chunk
      -- Do two things here: check that the location
      -- - has the right block id
      --    bat is a bytearraytag
      let (Just bytes) = getData $ fromJust $ moveToTag "Blocks" $ toZipper chunk :: Maybe B.ByteString
      let (BlockIds bids) = decode bytes :: BlockIds
      let expected = blockId block
      let actual = bids ! l
      assertEqual "BlockIds disagree: " expected actual

      let (Just bytes) = getData $ fromJust $ moveToTag "Data" $ toZipper chunk :: Maybe B.ByteString
      let (BlockData bd) = decode bytes :: BlockData
      let expected = blockDatum block
      let actual = bd ! l
      assertEqual "BlockDatas disagree: " expected actual
  
  
-- TODO This is a complete copy of the OneWoolBlock program.
-- Factor this out into its own function so that it can be invoked.
putOneBlock :: WorldDirectory -> Block -> IO CellCoords
putOneBlock dir block = do
  (Level level) <- decodeFile $ getLevelPath dir :: IO Level
  let playerC = fromJust $ getPlayerCoords level
  let cellCoord = playerToCellCoords playerC
  let woolCoord = fiveBlocksAbove cellCoord
  let changes = [(woolCoord, block)]
  performWorldUpdate dir changes
  return woolCoord

fiveBlocksAbove :: CellCoords -> CellCoords 
fiveBlocksAbove (x,z,y) = (x,z,y+5)

getBlockIds :: NBT -> B.ByteString
getBlockIds n = fromJust $ do
  z3 <- moveToTag "Blocks" (toZipper n)
  z4 <- down z3 :: Maybe (Zipper NBT)
  getHole z4 :: Maybe B.ByteString

getBlockData n = fromJust $ do
  z3 <- moveToTag "Data" (toZipper n)
  z4 <- down z3 :: Maybe (Zipper NBT)
  getHole z4 :: Maybe B.ByteString

-- Copy an entire directory recursively (Minecraft world)
makeCopy suffix dir = do
  let newDir = dir++suffix
  copyDirectoryRecursiveVerbose silent dir newDir
  return newDir

-- Arbitrary instances
-- ============================================================================
-- Region:
--   Create a whole bunch of chunks and put it into a Region.
--
-- For this, we create a completely arbitrary set of lists of fixed length.
instance Arbitrary Region where
  arbitrary = do
    chunks <- vector numChunksInRegion
    return . Region $ listArray ((0,0),(numChunksInRow-1,numChunksInCol-1)) chunks

instance Arbitrary (Compressed Chunk) where
  arbitrary = do
    liftM2 CompressedWith arbitrary arbitrary

instance Arbitrary CompressionFormat where
  arbitrary = elements [GZip,Zlib]

-- Create a random list of Word8.
instance Arbitrary BlockIds where
  arbitrary = do
    cells <- vector numCellsInChunk :: Gen [Word8]
    return . BlockIds $ listArray (arrMin,arrMax) cells
    where
      arrMin = (0,0,0)
      arrMax = (chunkSizeX-1, chunkSizeZ-1, chunkSizeY-1)

-- Create a random list of Word8...
instance Arbitrary BlockData where
  arbitrary = do
    cells <- vector numCellsInChunk :: Gen [Word8]
    let cells' = map (flip mod (2^4)) cells
    return . BlockData $ listArray (arrMin,arrMax) cells'
    where
      arrMin = (0,0,0)
      arrMax = (chunkSizeX-1, chunkSizeZ-1, chunkSizeY-1)
    
instance Arbitrary B.ByteString where
  arbitrary     = elements [B.cons 100 $ B.cons 200 B.empty, B.cons 100 B.empty]
-- instance Arbitrary B.ByteString where
--   arbitrary     = do
--     bytes <- arbitrary :: Gen [Word8]
--     guard $ (not.null) bytes
--     return $ B.pack bytes
