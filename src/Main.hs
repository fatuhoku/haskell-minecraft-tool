module Main where

main = undefined

-- Let's write code to put a white wool block 5 squares above the players'
-- head. fn points to a filepath.
-- - identify the exact region in which the player stands
-- - Read the file and seek to the appropriate chunk
-- putWool5MetresAbovePlayersHead :: FilePath -> IO ()
-- putWool5MetresAbovePlayersHead fn = do
--   p@(px,pz,py) <- return . getPlayerCoords =<< loadLevel (testWorld++"level.dat")
--   let rCoord = toRegionCoords . toChunkCoords $ p
--   withRegion testWorld rCoord (f p)
--   where
--     testWorld = "worlds/testworld/"
--     f = setWool White

-- TODO I wonder if any of these lines of code would be easier to write
-- with a generic zipper library such as syz?

-- We require that edits are made in the following form:
-- [(i,CompressedChunk)]
-- setWool :: WoolColour -> CellCoords -> Region -> Region
-- setWool colour cell (Region region) =
--   Region $ region // setWool' colour cell region
-- 
-- -- A list of re-mappings. We don't care about the original state of the chunk
-- setWool' :: WoolColour -> CellCoords -> Array ChunkCoords (Maybe CompressedChunk)
--          -> [(ChunkCoords, Maybe CompressedChunk)]
-- setWool' colour cell region = [(chunk, fmap (setWool'' colour cell chunk) (region ! chunk))]
--   where
--     chunk = toChunkCoords cell
-- 
-- -- Setting wool on an undefined chunk just does nothing.
-- setWool'' :: WoolColour -> CellCoords -> ChunkCoords -> CompressedChunk -> CompressedChunk
-- setWool'' colour cell chunk = modifyCc (setWool''' colour cell) cc 
-- 
-- setWool''' :: WoolColour -> CellCoords -> Chunk -> Chunk
-- setWool''' colour cell (CompoundTag (Just "Level") nbts) =
--   let zipper2 = undefined "Blocks" nbts in undefined

-- The way to recover the player's position (Spawn{X,Y,Z} coordinates)
-- from the saved game file is as follows:
--
-- Read in the level.dat
--     ! See
--     ! http://www.minecraftwiki.net/wiki/Alpha_Level_Format
--     - The path to this is
--       (C) _ / (C) Data / (C) Player / (I) Spawn{X,Y,Z}
--     - Confusingly, there is also a world spawn point
--       (C) _ / (C) Data / (I) Spawn{X,Y,Z}
-- 
-- Then, we determine what blocks we will need to render to by using a
-- rectangle class.
 

