module Tokens where

data TokenType = Space | OtherSpace | Fun | Op | Num | String | Char | OpenParen | CloseParen
	deriving (Show, Eq)
type Token = (TokenType, String)



