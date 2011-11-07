module Utils where

import Debug.Trace

ftrace = flip trace

-- Taken from hdirect, Utils
-- Returns Just v when v satisfies predicate.
toMaybe :: (a -> Bool) -> a -> Maybe a
toMaybe p x
  | p x  = Just x
	| otherwise = Nothing
