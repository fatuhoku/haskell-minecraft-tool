{-# LANGUAGE TemplateHaskell #-}
module Types where

{---------}
{- Types -}
{---------}
-- More descriptive type aliases that helps out with thinking about code.
import Data.Word

type Byte = Word8
type Nybble = Word8 -- wraps a Nybble
