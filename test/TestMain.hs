module Main where

import Test.LazySmallCheck
import Data.Binary
import System

import Types
import Region

-- Distribution.TestSuite requires that test frameworks (smallcheck, lazy-smallcheck)
-- provide instances for classes defined in Distribution.TestSuite.
--
-- cabal testsuite integration for lazysmallcheck is _not_ supported at the moment. 
-- 
-- Resorting to using exitcode-stdio mode for now. We will source a region file
-- from the test world and conduct the decEncDec test on it.
main :: IO ()
main = do
  putStrLn "Loading region from file..."
  region <- loadRegion testWorld (0,0)
  putStrLn "... finished loading region from file"
  putStrLn "Using region to check property..."
  exitWith $ if (prop_decEnc region)
    then ExitSuccess
    else ExitFailure 1
  where
    testWorld = "worlds/testworld"

{- SmallCheck property -}
prop_decEnc :: Region -> Bool
prop_decEnc r = decode (encode r) == r
