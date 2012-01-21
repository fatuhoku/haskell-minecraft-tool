{-# LANGUAGE MultiParamTypeClasses #-}
module Compression where

import qualified Data.ByteString.Lazy as B
import qualified Codec.Compression.GZip as G
import qualified Codec.Compression.Zlib as Z
import Data.Binary

-- The compressed type represents bytestrings that are 
-- we may need a phantom type to expose the underlying type.
-- The constructor is not exported.
data Compressed a = GZipCompressed B.ByteString | ZlibCompressed B.ByteString

-- This is a typeclass for Binary instances
-- So there's two steps: there is Binary, which is essentially our functor
-- instance, but there is also compression, which is also a functor instance.
-- The functory laws apply...
class BinaryFunctor b where
  bfmap :: (Binary a) => (a -> a) -> (b a -> b a)

-- So we've got a Compressed chunk object that we would simply like to get rid of.
-- The type system should convey that within a CompressedChunk is contained
-- within it a Chunk.
-- Question: Can we ever write something like "Compressed Chunk"?
-- Yes, if we are clear on one compression method.
-- In fact, This sort of implementation shows how useful parameterised modules
-- can be. Something which is found in OCaml but not in Haskell.
-- TODO example?
instance BinaryFunctor Compressed where
  bfmap f (GZipCompressed bs) = GZipCompressed $ G.compress.encode.f.decode.G.decompress $ bs
  bfmap f (ZlibCompressed bs) = ZlibCompressed $ Z.compress.encode.f.decode.Z.decompress $ bs
