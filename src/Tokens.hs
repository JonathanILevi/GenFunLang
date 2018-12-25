module Tokens where

data Token = Space Integer | Fun String | Op String | Num String | String String | Char String | Paren OpenClose
	deriving (Show)

data OpenClose = Open | Close
	deriving (Show)

