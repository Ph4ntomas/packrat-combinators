module Packrat.Char where

import Control.Applicative
import Data.Char

import Packrat.Prim
import Packrat.Combinators

anyChar :: Derivs d => Parser d Char
anyChar = Parser dvChar

char :: Derivs d => Char -> Parser d Char
char c = satisfy anyChar (\ch -> c == ch)

oneOf :: Derivs d => String -> Parser d Char
oneOf str = satisfy anyChar (flip elem str)

noneOf :: Derivs d => String -> Parser d Char
noneOf str = satisfy anyChar (flip notElem str)

string :: Derivs d => String -> Parser d String
string [] = return []
string (c:rem) = (:) <$> char c <*> (string rem)

oneStringOf :: Derivs d => [String] -> Parser d String
oneStringOf [str] = string str
oneStringOf (s:snxt) = string s <|> oneStringOf snxt

upper :: Derivs d => Parser d Char
upper = satisfy anyChar isUpper

lower :: Derivs d => Parser d Char
lower = satisfy anyChar isLower

alphaNum :: Derivs d => Parser d Char
alphaNum = satisfy anyChar isAlphaNum

digit :: Derivs d => Parser d Char
digit = satisfy anyChar isDigit

space :: Derivs d => Parser d Char
space = satisfy anyChar isSpace
