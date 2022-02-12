module Packrat.Prim (
  module Packrat.Internal.Derivs,

) where

import Control.Applicative
import Data.Semigroup as Semigroup (Semigroup(..))

import Packrat.Internal.Derivs
import Packrat.Errors
import Packrat.Positions

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
  empty = Parser (NoParse . nullError)

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

instance Derivs d => MonadFail (Parser d) where
  fail s = Parser (\dvs -> NoParse (PackError (dvPos dvs) [Message s]))

instance (Semigroup.Semigroup v, Derivs d) => Semigroup.Semigroup (Parser d v) where
  (<>) = liftA2 (Semigroup.<>)

unit :: Derivs d => a -> Parser d a
unit v = Parser (\d -> Parsed v d (nullError d))
