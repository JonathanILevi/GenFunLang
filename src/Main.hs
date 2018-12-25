import Prelude hiding (lex)
----import Lex
----import Chunk
----import CharGroups(charGroupsIs, individualCharsIs)
import ChunkToks
import Lex

code = "5* (5+sin(5.2))"

main = putStrLn $ show $ lex $ chunkToks code
----main = putStrLn $ show $ chunk isCharGroups code


