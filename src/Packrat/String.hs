module Packrat.String (
  string
  , oneOf
) where

import Control.Applicative ((<|>))

import Data.List

import Packrat.Prim
import Packrat.Errors
import Packrat.Char (char)

string :: Derivs d => String -> Parser d String
string s = foldr (\c -> (<*>) ((:) <$> char c)) (pure []) s <?> show s

oneOf :: Derivs d => [String] -> Parser d String
oneOf [] = Parser p
  where p d = NoParse $ internalError d "String.oneOf' was called without any strings"
oneOf strs = sub (sortOn (negate . length) strs) <?> ("one string of: " ++ unwords strs)
  where sub [] = undefined
        sub [str] = string str
        sub strs@(s:nxt) = string s <|> sub nxt
