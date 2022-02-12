module Packrat.String (
  string,
  oneOf
) where

import Control.Applicative ((<|>))

import Packrat.Prim
import Packrat.Errors
import Packrat.Char (char)

string :: Derivs d => String -> Parser d String
string s = foldr (\c -> (<*>) ((:) <$> char c)) (pure []) s <?> show s

oneOf :: Derivs d => [String] -> Parser d String
oneOf [] = Parser p 
  where p d = NoParse $ internalError d "oneStringOf was called without any strings"
oneOf [str] = string str
oneOf strs@(s:snxt) = (string s <|> oneOf snxt) <?> ("one string of: " ++ unwords strs)
