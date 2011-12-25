module Main where

-- import System
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
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit.Base hiding (Test)
import Test.QuickCheck
import qualified Data.ByteString.Lazy as B
import qualified Test.HUnit as H

import Access
import Block
import Chunk
import Coords
import Level
import FileIO
import Region
import NBTExtras
import Types
import Utils
import World

{- The LShift-Minecraft test suite.
 -
 - Properties and unit tests are tested here.
 -}
testSuite :: [Test]
testSuite = [testProperties, testUnit]

{-
 - Properties include checking the encoding and decoding of bytestrings
 - yield identity.
 -}
testProperties :: Test
testProperties =
  testGroup "Properties" [
        testProperty "propDecEncRegion" propDecEncRegion,
        testProperty "propToFromNybbles" propToFromNybbles,
        testProperty "propDecEncBlockIds" propDecEncBlockIds,
        testProperty "propDecEncBlockData" propDecEncBlockData
      ]

testUnit :: Test
testUnit = testGroup "Unit tests" [
--        testCase "testUpdateChangesBlock" testUpdateChangesBlock,
        testGroup "moveToTag" $
          concatMap hUnitTestToTests [
            testMoveToBlockId,
            testMoveToBlockData,
            testUpdateChangesBlockIds,
            testUpdateChangesBlockData,
            testOneBlockPlacesCorrectlyColouredWoolInWorld
          ]
        ]
  

-- A realistic test NBT for a level...
-- Let's make a chunk entirely full of gold;
-- and have our function set one square of it (rather specifically) to white
-- wool. So here's the chunk...
-- TODO
-- There is ambiguity in this sentence:
--   "The Data, BlockLight, and SkyLight arrays have four bits for each byte of the
--   Blocks array. The least significant bits of the first byte of the Data,
--   BlockLight, or SkyLight arrays correspond to the first byte of the Blocks
--   array."

{- Test NBT data -}
nbt1 = CompoundTag (Just "Level") [
  DoubleTag (Just "Zero") 0.0,
  ByteArrayTag (Just "Blocks") 2 (2 `B.cons` (1 `B.cons` B.empty)),
  IntTag (Just "One") 1,
  ByteArrayTag (Just "Data") 1 (1 `B.cons` B.empty)
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

testMoveToBlockId = "Move to Blocks tag" ~:
  getHole (fromJust $ moveToTag "Blocks" (toZipper nbt2)) ~?= Just chunkOfPureGold
testMoveToBlockData  = "Move to Data tag" ~:
  getHole (fromJust $ moveToTag "Data" (toZipper nbt2)) ~?= Just dataPureZero
 
{- Test cases -}


-- Put a white wool block in the gold block.
-- This only requires manipulation of the block data array.
testUpdateChangesBlockIds :: H.Test
testUpdateChangesBlockIds = "Put wool block in gold chunk" ~:
  (expectedBids == actualBids) ~? diffMsg
  where
    -- Get the blocks array of nbt2; expect the same array with one value
    -- changed at locC
    (BlockIds ids) = decode $ getBlockIds nbt2 :: BlockIds
    expectedBids@(BlockIds expectedArr) = BlockIds $ ids // [(locC,wool)]
    actualBids@(BlockIds actualArr) = decode $ getBlockIds nbt2' :: BlockIds
    nbt2' = updateChunk [updateBlocks] nbt2
    updateBlocks = blockIdUpdates [(locC,wool)]
    diffMsg = "The actual block data differs from expected. Here are some differences.\n"
              ++ show (diff 20 expectedArr actualArr) ++ "\n"
              ++ show (atMin expectedArr actualArr) ++ "\n"
    locC = (1,1,1)
    wool = 35

-- This test reads the BlockData from nbt2, produces nbt2' by updating its Data
-- then compares the expected array with the (nbt2+delta) updatedData...
testUpdateChangesBlockData :: H.Test
testUpdateChangesBlockData = "Put different coloured wool block in wool chunk!" ~:
  (expectedBd == actualBd) ~? diffMsg
  where
    expectedBd@(BlockData expectedArr) = BlockData $ d // [(locC,colour)]
    actualBd@(BlockData actualArr) = decode $ getBlockData nbt2' :: BlockData
    (BlockData d) = decode $ getBlockData nbt2 :: BlockData
    nbt2' = updateChunk [blockDataUpdates [(locC,colour)]] nbt2
    diffMsg = "The actual block data differs from expected. Here are some differences.\n"
              ++ show (diff 20 expectedArr actualArr) ++ "\n"
              ++ show (atMin expectedArr actualArr) ++ "\n"
    locC = (1,1,1)
    colour = 1

-- Copy the world into a new location (testworld_copy)
-- Run the putting of a block of wool five blocks above the player's head
-- Read the file again, and ensure that the black wool block is found, as
-- expected, on top of the player's head.
testOneBlockPlacesCorrectlyColouredWoolInWorld :: H.Test
testOneBlockPlacesCorrectlyColouredWoolInWorld = "Modify the world, and observe that it has changed" ~:
  TestCase $ do
    worldCopy <- makeCopy "_copy" "worlds/testworld"
    let testId = 35    -- wool
    let testDatum = 15 -- black
    let testBlock = Block testId testDatum
    woolCoord <- putOneBlock worldCopy testBlock
    assertBlock worldCopy woolCoord testBlock

makeCopy suffix dir = do
  let newDir = dir++suffix
  copyDirectoryRecursiveVerbose silent dir newDir
  return newDir

-- Asserts that a world has a particular block at a particular place.
assertBlock :: WorldDirectory -> CellCoords ->  Block -> IO ()
assertBlock dir cell block = do
  let (r,c,l) = toHierarchicalCoords cell
  (Region region) <- loadRegion dir r
  case region ! c of
    Nothing -> fail $ "No such region: " ++ show r
    Just cc -> do
      let chunk = chunkFromCc cc
      -- Do two things here: check that the location
      -- - has the right block id
      --    bat is a bytearraytag
      let (Just bytes) = getData $ fromJust $ moveToTag "Blocks" $ toZipper chunk :: Maybe B.ByteString
      let (BlockIds bids) = decode bytes :: BlockIds
      let expected = blockId block
      let actual = bids ! l
      putStrLn $ "The observed block Id is: " ++ show actual
      assertEqual "BlockIds disagree: " expected actual

      let (Just bytes) = getData $ fromJust $ moveToTag "Data" $ toZipper chunk :: Maybe B.ByteString
      let (BlockData bd) = decode bytes :: BlockData
      let expected = blockDatum block
      let actual = bd ! l
      putStrLn $ "The observed block data is: " ++ show actual
      assertEqual "BlockDatas disagree: " expected actual
  
  
-- This is a complete copy of the OneWoolBlock program - it puts one block 5
-- blocks above the player's head.
putOneBlock :: WorldDirectory -> Block -> IO CellCoords
putOneBlock dir block = do
  putStrLn "Reading level..."
  (Level level) <- decodeFile $ getLevelPath dir :: IO Level
  let playerC = fromJust $ getPlayerCoords level
  let cellCoord = playerToCellCoords playerC
  putStrLn "Extracted coordinates..."
  putStrLn $ "  player: " ++ show playerC
  let woolCoord = fiveBlocksAbove cellCoord
  let changes = [(woolCoord, block)]
  putStrLn "Updating region..."
  performWorldUpdate dir changes
  putStrLn "Done!"
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

{- Properties -}

-- I want a function that would tell me what part of the region is not the same.
-- One of the elements must necessarily be different.
-- 
-- TODO How many times is this property checked? I would like it checked 100
-- times.
--
-- Clearly an arbitrary bytestring should do. The array can be 
propDecEncRegion :: Region -> Bool
propDecEncRegion r@(Region reg) =
  let rt@(Region reg') = (decode.encode) r
  in if rt /= r
       then error $ "The first 10 pairs of differing entries':"
                    ++ show (diff 10 reg reg')
       else True

propToFromNybbles ::  Byte -> Bool
propToFromNybbles byte = (fromNybbles.toNybbles) byte == byte

propDecEncBlockIds ::  BlockIds -> Bool
propDecEncBlockIds bd@(BlockIds bds) =
  let rt@(BlockIds bds') = (decode.encode) bd
  in if bds' /= bds
       then error $ "The first 10 pairs of differing entries':"
                    ++ show (diff 10 bds bds')
       else True

propDecEncBlockData :: BlockData -> Bool
propDecEncBlockData bd@(BlockData bds) =
  let rt@(BlockData bds') = (decode.encode) bd
  in if bds /= bds'
       then error $ "The first 10 pairs of differing entries':"
                    ++ show (diff 10 bds bds')
       else True

-- Assuming that a and b are of the same shape...
-- Take the item at the minimum.
atMin a b
  | bounds a == bounds b = let (i,_) = bounds a in ((i,a!i), (i,b!i))
  | otherwise = error "The arrays' dimensions do not agree"
    

-- Computes the first n differences between two arrays.
diff n a b = take n $
  filter (\(a,b) -> snd a /= snd b) $ zip (assocs a) (assocs b)

-- Putting a block in a region will change it.
-- Not just any arbitrary CellCoords will do here it must be Bounded.
-- propPutBlockRegion :: Region -> CellCoords -> Block -> Bool
-- propPutBlockRegion r@(Region reg) coords block = undefined


-- Distribution.TestSuite requires that test frameworks (smallcheck, lazy-smallcheck)
-- provide instances for classes defined in Distribution.TestSuite.
--
-- cabal testSuiteuite integration for lazysmallcheck is _not_ supported at the moment. 
-- 
-- Resorting to using exitcode-stdio mode for now. We will source a region file
-- from the test world and conduct the decEncDec test on it.
main :: IO ()
main = defaultMain testSuite

-- INSTANCES --

-- Region:
-- An arbitrary region can be valid (all chunks are defined) as well as
-- invalid (there are missing chunks, or at least one of its chunks are invalid)
-- Make sure that functions that take Region have this as a precondition.
-- Choose out of valid or invalid regions:
--
-- For simplicity, let's generate completely arbitrary regions.
-- For this, we create a completely arbitrary set of lists of fixed length.
-- This can be done by using the Arbitrary Monad instance.
instance Arbitrary Region where
  arbitrary = do
    chunks <- vector numChunksInRegion
--    chunks <- sequence $ arbitrary:[return Nothing | _ <- [4..numChunksInRegion]] ++ [arbitrary,arbitrary]
    return . Region $ listArray ((0,0),(numChunksInRow-1,numChunksInCol-1)) chunks

instance Arbitrary CompressedChunk where
  arbitrary = do
    liftM3 CompressedChunk arbitrary arbitrary arbitrary

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
