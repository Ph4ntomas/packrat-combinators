module Packrat.Char where

import Control.Applicative
import Data.Char

import Packrat.Prim
import Packrat.Combinators
import Packrat.Errors

anyChar :: Derivs d => Parser d Char
anyChar = Parser dvChar

char :: Derivs d => Char -> Parser d Char
char c = satisfy anyChar (c==) <?> show c

oneOf :: Derivs d => String -> Parser d Char
oneOf str = satisfy anyChar (flip elem str) <?> ("one of : " ++ show str)

noneOf :: Derivs d => String -> Parser d Char
noneOf str = satisfy anyChar (flip notElem str)

string :: Derivs d => String -> Parser d String
string s = foldr (\c -> (<*>) ((:) <$> char c)) (pure []) s <?> show s

oneStringOf :: Derivs d => [String] -> Parser d String
oneStringOf [str] = string str
oneStringOf (s:snxt) = string s <|> oneStringOf snxt

upper :: Derivs d => Parser d Char
upper = satisfy anyChar isUpper <?> "uppercase character"

lower :: Derivs d => Parser d Char
lower = satisfy anyChar isLower <?> "lowercase character"

alphaNum :: Derivs d => Parser d Char
alphaNum = satisfy anyChar isAlphaNum <?> "alpanumeric character"

digit :: Derivs d => Parser d Char
digit = satisfy anyChar isDigit <?> "digit"

space :: Derivs d => Parser d Char
space = satisfy anyChar isSpace <?> "spacing"

eof :: Derivs d => Parser d ()
eof = notFollowdBy anyChar <?> "EOF"
