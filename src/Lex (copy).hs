module Lex (lex) where
import Prelude hiding (lex)

import Tokens
import CharGroups


lex :: String -> [Token]
lex [] = []
lex code = tok : lex remainingCode
	where (tok, remainingCode) = takeTok code

takeTok :: String -> (Token, String)
takeTok (x:xs)
	| isSpace x	= (	Whitespace $ toInteger $ length (x : takeWhile isSpace xs)	,
			dropWhile isSpace xs	)
		
	| isDigit x	= if head (dropWhile isDigit xs) == '.'
			then (	Decimal 0.1	,  ---- $ takeWhile isDigit $ tail (dropWhile isDigit xs)
				dropWhile isDigit $ tail (dropWhile isDigit xs)	)
			else (	NonDecimal 1	,
				(dropWhile isDigit xs)	)
		
	| isAlpha x	= (	Function $ x : takeWhile (isAlpha >||< isDigit >||< (=='_')) xs	,
			dropWhile (isAlpha >||< isDigit >||< (=='_')) xs	)
	| isSymbol x	= (	Function $ x : takeWhile isSymbol xs	,
			dropWhile isSymbol xs	)
	| x == '('	= (	Paren Open, xs	)
	| x == ')'	= (	Paren Close, xs	)



----takeTok :: String -> (Token, String)
----takeTok ('5':xs) = (Number 5, xs)
----takeTok ('*':xs) = (Function "*", xs)
----takeTok ('s':'i':'n':xs) = (Function "sin", xs)
----takeTok ('(':xs) = (Paren Open, xs)
----takeTok (')':xs) = (Paren Close, xs)
----takeTok (' ':xs) = (Whitespace 1, xs)
----takeTok xs = (None, tail xs)


(>||<) a b c = a c || b c

takeDropWhile x y = (takeWhile x y, dropWhile x y)


if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y