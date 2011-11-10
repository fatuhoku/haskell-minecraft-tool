module Utils where

import Data.Monoid
import Text.Printf
import Debug.Trace

ftrace = flip trace

-- Traces a value, accepting a message to display in front of the value.
-- "Forks" the input to trace, and feeds it into the next function application
vtrace :: (Show a) => String -> a -> a
vtrace msg val = trace (msg ++ show val ++ "\n") val

funtrace :: (Show a) => String -> (a -> String) -> [a] -> [a]
funtrace msg f val = trace (msg ++ show (map f val) ++ "\n") val

-- Taken from hdirect, Utils
-- Returns Just v when v satisfies predicate.
toMaybe :: (a -> Bool) -> a -> Maybe a
toMaybe p x
  | p x  = Just x
	| otherwise = Nothing
