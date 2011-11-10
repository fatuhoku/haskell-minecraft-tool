module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck
import QuickCheckUtils
import Test.QuickCheck
import System

import Control.Monad

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import Data.Array
import Data.Binary
import Crypto.Random
import Crypto.Types

import Types
import Region
import Constants

tests = [
    testGroup "Properties" [
        testProperty "Region: decode . encode == id" prop_decEncRegion
      ]
  ]

{- Properties
 - Since we want r to be evaluated once and once only, we will make r shared -}
prop_decEncRegion :: Region -> Bool
prop_decEncRegion r = (decode . encode) r == r

-- Distribution.TestSuite requires that test frameworks (smallcheck, lazy-smallcheck)
-- provide instances for classes defined in Distribution.TestSuite.
--
-- cabal testsuite integration for lazysmallcheck is _not_ supported at the moment. 
-- 
-- Resorting to using exitcode-stdio mode for now. We will source a region file
-- from the test world and conduct the decEncDec test on it.
main :: IO ()
main = defaultMain tests


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
  coarbitrary = undefined
  arbitrary = do
    chunks <- sequence [arbitrary | _ <- [1..numChunksInRegion]]
    return . Region $ listArray ((0,0),(numChunksInRow-1,numChunksInCol-1)) chunks

instance Arbitrary CompressedChunk where
  coarbitrary = undefined
  arbitrary = do
    liftM3 CompressedChunk arbitrary arbitrary arbitrary

instance Arbitrary CompressionFormat where
  coarbitrary = undefined
  arbitrary = elements [GZip,Zlib]

-- instance Arbitrary B.ByteString where
--   coarbitrary = undefined
--   arbitrary = do
--     len <- arbitrary :: Gen ByteLength
--     randStr <- arbitrary :: Gen [Word8]
--     -- Hmm. This is really not doing well.
--     let Right gen = newGen $ BS.concat $ B.toChunks $ B.pack randStr
--     let Right (bs, gen') = genBytes len gen :: Either GenError (BS.ByteString, SystemRandom)
--     return $ B.fromChunks [bs]
--
instance Arbitrary B.ByteString where
  coarbitrary = undefined
  arbitrary = elements [1 `B.cons` B.empty, B.empty]

instance Arbitrary Word8 where
  coarbitrary = undefined
  arbitrary = do
    x <- arbitrary :: Gen Int
    return . fromIntegral $ x `mod` 2^8

instance Arbitrary Word32 where
  coarbitrary = undefined
  arbitrary = do
    x <- arbitrary :: Gen Int
    return . fromIntegral $ x `mod` 2^32
