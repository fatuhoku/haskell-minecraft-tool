module Utils where

{---------}
{- Utils -}
{---------}
-- Common utility functions to keep me sane.

import Data.Bits
import Data.Monoid
import Debug.Trace
import Text.Printf

import Types

ftrace = flip trace

-- Traces a value, accepting a message to display in front of the value.
-- "Forks" the input to trace, and feeds it into the next function application
vtrace :: (Show a) => String -> a -> a
vtrace msg val = trace (msg ++ show val) val

funtrace :: (Show a) => String -> (a -> String) -> [a] -> [a]
funtrace msg f val = trace (msg ++ show (map f val)) val

-- Taken from hdirect, Utils
-- Returns Just v when v satisfies predicate.
toMaybe :: (a -> Bool) -> a -> Maybe a
toMaybe p x
  | p x  = Just x
	| otherwise = Nothing

-- Unpacks Byte to a pair of nybbles.
-- Note: in Minecraft, the Data, Skylight etc. arrays
-- have the least significant nybble first. Make sure the result of this
-- function is swapped before proceeding
toNybbles :: Byte -> (Nybble,Nybble)
toNybbles b = (b `shiftR` 4, b `mod` (2^4))

-- Precondition is that both input nybbles need to be under 128.
-- Note: in Minecraft, the Data, Skylight etc. arrays
-- have the least significant nybble first. Make sure the argument of the
-- function has its most significant bytes first.
fromNybbles :: (Nybble,Nybble) -> Byte 
fromNybbles (n1,n2) = (n1 `shiftL` 4) + (n2 `mod` (2^4))

tupleToList :: (a,a) -> [a]
tupleToList (a,b) = [a,b]

