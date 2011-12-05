module Main where

-- import System
import Control.Monad
import Data.Array
import Data.Binary
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import qualified Data.ByteString.Lazy as L

import Block
import Chunk
import Coords
import Region
import Types
import Utils

{- The LShift-Minecraft test suite.
 -
 - Properties and unit tests are tested here.
 -}
testSuite :: [Test]
testSuite = [testProperties]

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
  let rt@(Region roundTrip) = (decode.encode) r
      differingEntries = filter (\(a,b) -> snd a /= snd b) $ zip (assocs reg) (assocs roundTrip)
  in if rt /= r then error $ "The first 10 pairs of differing entries':" ++ show (take 10 differingEntries)
                    else True

propToFromNybbles ::  Byte -> Bool
propToFromNybbles byte = (fromNybbles.toNybbles) byte == byte

propDecEncBlockIds ::  BlockIds -> Bool
propDecEncBlockIds bd@(BlockIds bdArr) =
  let rt@(BlockIds roundTrip) = (decode.encode) bd
      differingEntries = filter (\(a,b) -> snd a /= snd b) $ zip (assocs bdArr) (assocs roundTrip)
  in if rt /= bd
      then error $ "The first 10 pairs of differing entries':"
                   ++ show (take 10 differingEntries)
      else True

propDecEncBlockData :: BlockData -> Bool
propDecEncBlockData bd@(BlockData bdArr) =
  let rt@(BlockData roundTrip) = (decode.encode) bd
      differingEntries = filter (\(a,b) -> snd a /= snd b) $ zip (assocs bdArr) (assocs roundTrip)
  in if rt /= bd then error $ "The first 10 pairs of differing entries':" ++ show (take 10 differingEntries)
                    else True


-- Putting a block in a region will change it.
-- Not just any arbitrary CellCoords will do here it must be Bounded.
propPutBlockRegion :: Region -> CellCoords -> Block -> Bool
propPutBlockRegion r@(Region reg) coords block = undefined


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
