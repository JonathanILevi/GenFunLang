module Gen.ChunkMatching (chunkMatching) where


chunkMatching :: [(a->Bool)] -> [a] -> [[a]]
chunkMatching _ [] = []
chunkMatching fs xs = c : chunkMatching fs remaining
	where (c, remaining) = takeChunk fs xs

takeChunk :: [(a->Bool)] -> [a] -> ([a],[a])
takeCunnk [] _ = []
takeChunk (f:fs) all@(x:xs) =
	if (f x)
		then takeDropWhile f all
		else takeChunk fs all

takeDropWhile x y = (takeWhile x y, dropWhile x y)
