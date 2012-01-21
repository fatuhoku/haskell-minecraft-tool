module Main where

main = undefined

-- TODO I wonder if any of these lines of code would be easier to write
-- with a generic zipper library such as syz?

-- We require that edits are made in the following form:
-- [(i,CompressedChunk)]
-- setWool :: WoolColour -> CellCoords -> Region -> Region
-- setWool colour cell (Region region) =
--   Region $ region // setWool' colour cell region

