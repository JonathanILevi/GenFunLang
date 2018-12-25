import Prelude hiding (lex)

import Lex

code = "5* (5+sin(5.2))"

main = putStrLn $ show $ lex code


