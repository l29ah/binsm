module Maps where

import Data.Int
import Data.Maybe
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
	return $ Just $ Part (rh st) (rh en) name

emptyLine = newline >> pure Nothing

dmesg :: String -> [Part]
dmesg s = catMaybes $ either (error . show) id $ parse (many1 (dmesgLine <|> emptyLine)) "" s
