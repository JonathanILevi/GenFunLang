module Lex (lex) where
import Prelude hiding (lex)

import Tokens
import CharGroups hiding (CharType(Space))


lex :: String -> [Token]
lex [] = [];
lex x = tok : lex remaining
	where (tok, remaining) = takeTok x

takeTok :: String -> (Token, String)
takeTok a@(x:_) | isSpace x || isTab x || isNewLine x	= tokSpace	a
takeTok a@(x:_) | isAlpha x	= tokFun	a
takeTok a@(x:_) | isSymbol x	= tokOp	a
takeTok a@(x:_) | isDigit x	= tokNum	a
takeTok a@(x:_) | isOpenParen x || isCloseParen x	= tokParen	a

tokSpace	(x:xs)	= (createSpace	$ x:forTok, remaining)	where (forTok,remaining) = takeDropWhile (\x2	-> isSpace x2 || isTab x2 || isNewLine x2 	)	xs
tokFun	(x:xs)	= (createFun	$ x:forTok, remaining)	where (forTok,remaining) = takeDropWhile (\x2	-> isAlpha x2 || isDigit x2	)	xs
tokOp	(x:xs)	= (createOp	$ x:forTok, remaining)	where (forTok,remaining) = takeDropWhile (\x2	-> isSymbol x2	)	xs
tokNum	(x:xs)	= (createNum	$ x:forTok, remaining)	where (forTok,remaining) = takeDropWhile (\x2	-> isDigit x2 || isAlpha x2 || isDot x2	)	xs
tokParen	(x:xs)	= (createParen	$ x:forTok, remaining)	where (forTok,remaining) = ([],xs)

createSpace :: String -> Token
createSpace x
	| any isTab x	= Space (-1)
	| any isNewLine x	= Space (-1)
	| otherwise	= Space (fromIntegral $ length x)

createFun :: String -> Token
createFun	x = Fun	x
createOp	x = Op	x
createNum	x = Num	x
createString	x = String	x
createChar	x = Char	x

createParen x
	| x == "(" = Paren Open
	| x == ")" = Paren Close



takeDropWhile x y = (takeWhile x y, dropWhile x y)
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y


