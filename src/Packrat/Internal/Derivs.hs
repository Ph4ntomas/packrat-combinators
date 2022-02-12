module Packrat.Internal.Derivs (
  Derivs(..)
  , Result(..)
  , Parser(..)
) where

import Packrat.Positions
import Packrat.Internal.Errors (PackError)

class Derivs d where
  dvPos :: d -> PackPos
  dvChar :: d -> Result d Char
  dvInput :: d -> String

data Result d v =  Parsed v d PackError |
                                NoParse PackError

newtype Parser d v = Parser (d -> Result d v)
