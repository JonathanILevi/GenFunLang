module Lex (lex) where
import Prelude hiding (lex)

import Tokens hiding (TokenType)
import CharGroups hiding (CharType)
import Tokens (TokenType)
import CharGroups (CharType)
import qualified Tokens as T
import qualified CharGroups as C


lex :: [(CharType, String)] -> [Token]
lex [] = [];
lex x = tok : lex remaining
	where (tok, remaining) = takeTok x

takeTok :: [(CharType, String)] -> (Token, [(CharType,String)])
takeTok a@((x,_):_) | x==C.Space	= tokSpace	a
takeTok a@((x,_):_) | x==C.Tab || x==C.NewLine	= tokOtherSpace	a
takeTok a@((x,_):_) | x==C.Alpha	= tokFun	a
takeTok a@((x,_):_) | x==C.Symbol	= tokOp	a
takeTok a@((x,_):_) | x==C.Digit	= tokNum	a
takeTok a@((x,_):_) | x==C.OpenParen	= tokOpenParen	a
takeTok a@((x,_):_) | x==C.CloseParen	= tokCloseParen	a

tokSpace	(x:xs)	= ((T.Space	, concat $ map snd $ x:forTok), remaining)	where (forTok,remaining) = takeDropWhile (\(x2,_)	-> x2==C.Space)	xs
tokOtherSpace	(x:xs)	= ((T.OtherSpace	, concat $ map snd $ x:forTok), remaining)	where (forTok,remaining) = takeDropWhile (\(x2,_)	-> x2==C.Tab || x2==C.NewLine)	xs
tokFun	(x:xs)	= ((T.Fun	, concat $ map snd $ x:forTok), remaining)	where (forTok,remaining) = takeDropWhile (\(x2,_)	-> x2==C.Alpha || x2==C.Digit)	xs
tokOp	(x:xs)	= ((T.Op	, concat $ map snd $ x:forTok), remaining)	where (forTok,remaining) = takeDropWhile (\(x2,_)	-> x2==C.Symbol)	xs
tokNum	(x:xs)	= ((T.Num	, concat $ map snd $ x:forTok), remaining)	where (forTok,remaining) = takeDropWhile (\(x2,y2)	-> x2==C.Digit || x2==C.Alpha || all (=='.') y2)	xs
tokOpenParen	(x:xs)	= ((T.OpenParen	, concat $ map snd $ x:forTok), remaining)	where (forTok,remaining) = takeDropWhile (\(x2,y2)	-> x2==C.OpenParen)	xs
tokCloseParen	(x:xs)	= ((T.CloseParen	, concat $ map snd $ x:forTok), remaining)	where (forTok,remaining) = takeDropWhile (\(x2,y2)	-> x2==C.CloseParen)	xs


takeDropWhile x y = (takeWhile x y, dropWhile x y)
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y
