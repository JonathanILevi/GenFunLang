module ChunkToks (chunkToks) where

import Gen.LToR
import Gen.ChunkMatching
import CharGroups(charGroupsIs, individualCharsIs, charGroup)
import FunArray
import Data.List.Split (chunk)


chunkToks code = code & group & separate & addType

group	x = chunkMatching charGroupsIs x
separate	x = concat $ map subSplit $ x
addType	x = map (\y -> (charGroup y, y)) x

subSplit xs = if (funOr individualCharsIs) (head xs) then chunk 1 xs else [xs]
