{-# LANGUAGE MultiParamTypeClasses #-}
module Compression where

import qualified Data.ByteString.Lazy as B
import qualified Codec.Compression.GZip as G
import qualified Codec.Compression.Zlib as Z
import Data.Binary

-- We use phantom type variable 'a' in (Compressed a) to wrap work with Bytestrings
-- that, when decompressed and decoded, represent data of type 'a'.
data CompressionFormat = GZip | Zlib deriving (Eq, Show)
data Compressed a = CompressedWith CompressionFormat B.ByteString
  deriving (Eq,Show)

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
  bfmap f (CompressedWith GZip bs) = CompressedWith GZip $ G.compress.encode.f.decode.G.decompress $ bs
  bfmap f (CompressedWith Zlib bs) = CompressedWith Zlib $ Z.compress.encode.f.decode.Z.decompress $ bs

instance Binary CompressionFormat where
  get = getWord8 >>= \n -> return $ g n
    where 
      g 1 = GZip
      g 2 = Zlib
      g x = error $ "get CompressionFormat: unsupported compression number: " ++ show x
  put GZip = putWord8 1
  put Zlib = putWord8 2
