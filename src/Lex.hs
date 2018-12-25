module Lex (lex) where
import Prelude hiding (lex)

import Tokens hiding (TokenType)
import CharGroups hiding (CharType)
import Tokens (TokenType)
import CharGroups (CharType)
import qualified Tokens as T
import qualified CharGroups as C


lex :: String -> [Token]
lex [] = [];
lex x = tok : lex remaining
	where (tok, remaining) = takeTok x

takeTok :: String -> (Token, String)
takeTok a@(x:_) | isSpace x	= tokSpace	a
takeTok a@(x:_) | isTab x || isNewLine x	= tokOtherSpace	a
takeTok a@(x:_) | isAlpha x	= tokFun	a
takeTok a@(x:_) | isSymbol x	= tokOp	a
takeTok a@(x:_) | isDigit x	= tokNum	a
takeTok a@(x:_) | isOpenParen x	= tokOpenParen	a
takeTok a@(x:_) | isCloseParen x	= tokCloseParen	a

tokSpace	(x:xs)	= ((T.Space	, x:forTok), remaining)	where (forTok,remaining) = takeDropWhile (\x2	-> isSpace x2	)	xs
tokOtherSpace	(x:xs)	= ((T.OtherSpace	, x:forTok), remaining)	where (forTok,remaining) = takeDropWhile (\x2	-> isTab x2 || isNewLine x2	)	xs
tokFun	(x:xs)	= ((T.Fun	, x:forTok), remaining)	where (forTok,remaining) = takeDropWhile (\x2	-> isAlpha x2 || isDigit x2	)	xs
tokOp	(x:xs)	= ((T.Op	, x:forTok), remaining)	where (forTok,remaining) = takeDropWhile (\x2	-> isSymbol x2	)	xs
tokNum	(x:xs)	= ((T.Num	, x:forTok), remaining)	where (forTok,remaining) = takeDropWhile (\x2	-> isDigit x2 || isAlpha x2 || (x2=='.')	)	xs
tokOpenParen	(x:xs)	= ((T.OpenParen	, x:forTok), remaining)	where (forTok,remaining) = ([],xs)
tokCloseParen	(x:xs)	= ((T.CloseParen	, x:forTok), remaining)	where (forTok,remaining) = ([],xs)


takeDropWhile x y = (takeWhile x y, dropWhile x y)
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y
