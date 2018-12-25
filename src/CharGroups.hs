module CharGroups where
import qualified Prelude
import Prelude ((==),(||))
import qualified Data.Char (isDigit, isAlpha) 

charGroupsIs =	[ isSpace	
	, isTab	
	, isNewLine	
	, isAlpha	
	, isDigit	
	, isDot	
	, isSymbol	
	, isOpenParen	
	, isCloseParen	]
individualCharsIs =	[ isOpenParen	
	, isCloseParen	]

isSpace	x	= x==' '
isTab	x	= x=='\t'
isNewLine	x	= x=='\n'
isAlpha	x	= Data.Char.isAlpha x || x=='_'
isDigit	x	= Data.Char.isDigit x
isDot	x	= x=='.'
isSymbol	x 	= Prelude.elem x symbols
isOpenParen	x 	= x=='('
isCloseParen	x 	= x==')'

symbols	= "-+/*%^~?!=&@$|."





charGroup (x:xs)
	| isSpace	 x	= Space	
	| isTab	 x	= Tab	
	| isNewLine	 x	= NewLine	
	| isAlpha	 x	= Alpha	
	| isDigit	 x	= Digit	
	| isDot	 x	= Dot	
	| isSymbol	 x	= Symbol	
	| isOpenParen	 x	= OpenParen	
	| isCloseParen	 x	= CloseParen	

data CharType =	  Space	
	| Tab	
	| NewLine	
	| Alpha	
	| Digit	
	| Dot	
	| Symbol	
	| OpenParen	
	| CloseParen	
	deriving (Prelude.Show, Prelude.Eq)

