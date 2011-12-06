module Main where

-- import System
import Control.Monad
import Data.Array
import Data.Binary
import Data.List
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import qualified Data.ByteString.Lazy as L
import qualified Test.HUnit as H

import Access
import Block
import Chunk
import Coords
import Region
import Types
import Utils
import Data.NBT
import Data.Int
import Test.HUnit.Base hiding (Test)
import Data.Maybe
import Data.Generics.Zipper

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
          concatMap hUnitTestToTests [testMoveToBlock, testMoveToData, testUpdateChangesBlock]
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

-- A simple NBT representing a 16x128x16 block of pure gold.
nbt2 :: NBT 
nbt2 = CompoundTag (Just "Level") [
  blocksPureGold,
  dataPureZero
  ]

-- Interesting; even though I've set all the blocks to pure gold, 9 appears.
blocksPureGold =
  let gold = 41 :: Word8
      size = fromIntegral numCellsInChunk
      bs = L.pack (replicate numCellsInChunk gold) in
  ByteArrayTag (Just "Blocks") size bs

-- This represents white wool, or the default data value for any block.
-- Create from numCellsInChunk nybbles of 0.
-- Encoding puts it back into a bytestring.
dataPureZero = let zeros = L.pack $ replicate size 0
                   size  = numCellsInChunk `div` 2
                   size' = fromIntegral size :: Int32 in
  ByteArrayTag (Just "Data") size' zeros

testMoveToBlock = "Move to Blocks tag" ~:
  getHole (fromJust $ moveToTag "Blocks" (toZipper nbt2)) ~?= Just blocksPureGold
testMoveToData  = "Move to Data tag" ~:
  getHole (fromJust $ moveToTag "Data" (toZipper nbt2)) ~?= Just dataPureZero
 
{- Test cases -}

-- Put a white wool block in the gold block.
-- It looks like the entire business of reading BlockData seems wrong.
-- It's yielding totally the wrong values!
testUpdateChangesBlock :: H.Test
testUpdateChangesBlock = (expectedBd == actualBd) ~? diffMsg
  where
    expectedBd@(BlockData expectedArr) = BlockData $ d // [(locC,wool)]
    actualBd@(BlockData actualArr) = decode $ getBlocks nbt2' :: BlockData
    nbt2' = updateBlocks nbt2
    updateBlocks = setBlockId locC wool
    (BlockData d) = decode $ getBlocks nbt2 :: BlockData
    diffMsg = show (diff 20 expectedArr actualArr) ++ "\n"
                     ++ show (atMin expectedArr actualArr)
    locC = (1,1,1)
    wool = 35

getBlocks :: NBT -> L.ByteString
getBlocks n = fromJust $ do
  z3 <- moveToTag "Blocks" (toZipper n)
  z4 <- down z3 :: Maybe (Zipper NBT)
  getHole z4 :: Maybe L.ByteString

getData n = fromJust $ do
  z3 <- moveToTag "Data" (toZipper n)
  z4 <- down z3 :: Maybe (Zipper NBT)
  getHole z4 :: Maybe L.ByteString

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
    
instance Arbitrary L.ByteString where
  arbitrary     = elements [L.cons 100 $ L.cons 200 L.empty, L.cons 100 L.empty]
-- instance Arbitrary L.ByteString where
--   arbitrary     = do
--     bytes <- arbitrary :: Gen [Word8]
--     guard $ (not.null) bytes
--     return $ L.pack bytes
