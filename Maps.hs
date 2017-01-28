module Maps where

import Data.Int
import Numeric
import Text.Parsec

data Part = Part
	{ start :: Int64
	, end :: Int64
	, name :: String
	} deriving Show

rh = fst . head . readHex

dmesgLine = do
	string "0x"
	st <- many1 hexDigit
	string "-0x"
	en <- many1 hexDigit
	string " : \""
	name <- many $ noneOf "\""
	string "\""
	newline
	return $ Part (rh st) (rh en) name

dmesg :: String -> [Part]
dmesg s = let (Right res) = parse (many1 dmesgLine) "" s in res
