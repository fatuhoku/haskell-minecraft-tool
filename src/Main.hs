import Data.NBT

import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as B
import Data.Binary ( Binary (..), decode, encode )
import qualified Data.ByteString.Lazy.UTF8 as UTF8 ( fromString, toString )
import Data.List

import Control.Applicative
import Control.Monad
import qualified Text.Show.Pretty as Pr

import System.Directory

-- We would very much like to print out the player coordinates.
-- Let's try doing what the test code does to extract the player location.
-- 
-- In the test file, fileL is the lazy file handle; we can force the whole decompression to
-- occur by doing B.pack . B.unpack $ fileL.
main = do
  dec <- loadLevel "worlds/testworld/level.dat"
  -- let coord = getPlayerCoordinates dec
  -- putStr $ show coord
  putStrLn $ Pr.ppShow dec

loadLevel fn = do
  currDir <- getCurrentDirectory
  fileL <- GZip.decompress <$> B.readFile fn
  let file = B.pack (B.unpack fileL)
      dec = (decode file :: NBT)
      enc = encode dec
  return dec
 
-- TODO what's the first argument to CompoundTag?
-- The spawn coordinates are always given in order. We can expect this.
getPlayerCoordinates :: NBT -> (Int, Int, Int)
getPlayerCoordinates (CompoundTag _ tags) =
  let [x,y,z] = map getInt $ filter (\t -> "Spawn" `isPrefixOf` getName t) tags
  in (x,y,z)
getPlayerCoordinates _ = error "Invalid level.dat NBT: does not begin with Data CompoundTag"

getName = undefined
getInt  = undefined
