module Packrat.Prim where

import Control.Applicative
import Data.List

import Packrat.Positions

-- Error Descriptor
data PackEDesc =  Expected String
                | Message String

-- Error Wrapper
data PackError = PackError {
                  pos :: PackPos,
                  descs :: [PackEDesc]
}

-- Result
data Result d v = Parsed v d PackError |
                  NoParse PackError

newtype Parser d v = Parser (d -> Result d v)

class Derivs d where
  dvPos :: d -> PackPos
  dvChar :: d -> Result d Char

instance Derivs d => Functor (Parser d) where
  fmap f (Parser p) = Parser parse where 
    parse dvs = case p dvs of
      Parsed v rem err -> Parsed (f v) rem err
      NoParse err -> NoParse err

instance Derivs d => Applicative (Parser d) where
  pure = unit

  (Parser lhs) <*> (Parser rhs) = Parser left_hand where 
    left_hand dvs = case lhs dvs of
      lhs_res@(Parsed f rem err) -> right_hand lhs_res
      NoParse e -> NoParse e
    right_hand (Parsed f rem err) = case rhs rem of
      Parsed v rem' err' -> Parsed (f v) rem' (joinErrors err err')
      NoParse err' -> NoParse (joinErrors err err')

instance Derivs d => Alternative (Parser d) where
  empty = Parser (\d -> NoParse (nullError d))

  (Parser lhs) <|> (Parser rhs) = Parser left_hand where
    left_hand dvs = case lhs dvs of
      lhs_res@(Parsed v rem err) -> lhs_res
      NoParse e -> right_hand e dvs
    right_hand el dvs = case rhs dvs of
      rhs_res@(Parsed v rem err) -> Parsed v rem (joinErrors el err)
      NoParse er -> NoParse (joinErrors el er)

instance Derivs d => Monad (Parser d) where
  return = unit

  (Parser lhs) >>= f = Parser left_hand where
    left_hand dvs = case lhs dvs of
      Parsed v rem err -> 
        let Parser rhs = f v
        in right_hand err (rhs rem)
      NoParse e -> NoParse e
    right_hand errl (Parsed v rem' err) = Parsed v rem' (joinErrors errl err)
    right_hand errl (NoParse err) = NoParse (joinErrors err errl)

unit :: Derivs d => a -> Parser d a
unit v = Parser (\d -> Parsed v d (nullError d))

instance Eq PackEDesc where
  Expected lhs == Expected rhs = lhs == rhs
  Message lhs == Message rhs = lhs == rhs
  _ == _ = False

joinErrors :: PackError -> PackError -> PackError
joinErrors (el@(PackError pl dl)) (er@(PackError pr dr))
  | pl < pr || null dl = er
  | pr < pl || null dr = el
  | otherwise = PackError pl (union dl dr)

nullError :: Derivs d => d -> PackError
nullError dvs = PackError (dvPos dvs) []
