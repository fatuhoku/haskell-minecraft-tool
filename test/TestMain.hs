module Main where

import Data.Array
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Data.Binary
import qualified Data.ByteString.Lazy as L
import System
import Control.Monad

import Utils
import Types
import Region
import Constants

tests = [
    testGroup "Properties" [
        testProperty "Region: decode . encode == id" prop_decEncRegion
      ]
  ]

{- Properties -}
-- I want a function that would tell me what part of the region is not the same.
-- One of the elements must necessarily be different.
prop_decEncRegion :: Region -> Bool
prop_decEncRegion r@(Region reg) =
  let rt@(Region roundTrip) = (decode . encode) r
      differingEntries = filter (\(a,b) -> snd a /= snd b) $ zip (assocs reg) (assocs roundTrip)
  in if rt /= r then error $ "The first 10 pairs of differing entries':" ++ show (take 10 differingEntries)
                    else True

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
  arbitrary = do
    chunks <- sequence [arbitrary | _ <- [1..numChunksInRegion]]
--    chunks <- sequence $ arbitrary:[return Nothing | _ <- [4..numChunksInRegion]] ++ [arbitrary,arbitrary]
    return . Region $ listArray ((0,0),(numChunksInRow-1,numChunksInCol-1)) chunks

instance Arbitrary CompressedChunk where
  arbitrary = do
    liftM3 CompressedChunk arbitrary arbitrary arbitrary

instance Arbitrary CompressionFormat where
  arbitrary = elements [GZip,Zlib]

instance Arbitrary L.ByteString where
  arbitrary     = return $ L.cons 1 L.empty
-- instance Arbitrary L.ByteString where
--   arbitrary     = do
--     bytes <- arbitrary :: Gen [Word8]
--     guard $ (not.null) bytes
--     return $ L.pack bytes
