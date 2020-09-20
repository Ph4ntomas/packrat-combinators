module Packrat.Prim where

import Control.Applicative
import Data.Semigroup as Semigroup (Semigroup(..))
import Data.List

import Packrat.Positions

-- Error Descriptor
data PackEDesc =  Expected    String
                | Unexpected  String
                | Message     String

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

instance Derivs d => MonadFail (Parser d) where
  fail s = Parser (\dvs -> NoParse (PackError (dvPos dvs) [Message s]))

instance (Semigroup.Semigroup v, Derivs d) => Semigroup.Semigroup (Parser d v) where
  (<>) = liftA2 (Semigroup.<>)

unit :: Derivs d => a -> Parser d a
unit v = Parser (\d -> Parsed v d (nullError d))

instance Eq PackEDesc where
  Expected lhs == Expected rhs = lhs == rhs
  Message lhs == Message rhs = lhs == rhs
  _ == _ = False

instance Show PackError where
  show (PackError pos []) = show pos ++ "parse error"
  show (PackError pos descs) = 
    prepPos (showExpecteds (getExpects descs)) ++
    prepPos (showUnexpecteds (getUnexpects descs)) ++ 
    prepPos (showMessages (getMessages descs))
    where
      prepPos [] = []
      prepPos s = show pos ++ s

showExpecteds [] = []
showExpecteds [Expected msg] = msg ++ " expected.\n"
showExpecteds [Expected msg, Expected msg'] = "Either " ++ 
  show msg ++ " or " ++ 
  show msg' ++ " expected.\n"
showExpecteds exps = "Expecting one of:\n" ++ expectList exps where 
  expectList [] = []
  expectList (Expected msg:tl) = "  " ++ show msg ++ ",\n" ++ expectList tl

showUnexpecteds [] = []
showUnexpecteds [Unexpected msg] = msg ++ " unexpected."
-- Following patterns make no sense because we should know what was unexpected here.
showUnexpecteds [Unexpected msg, Unexpected msg'] = "Either " ++
  show msg ++ " or " ++ 
  show msg' ++ " parsed but not expected.\n"
showUnexpecteds exps = "One of several unexpected token found:\n" ++ unexpectList exps where 
  unexpectList [] = []
  unexpectList (Unexpected msg:tl) = "  " ++ show msg ++ ",\n"

showMessages [] = []
showMessages [Message msg] = msg ++ "\n"
showMessages msgs = "Several error messages exists :\n" ++ msgList msgs where
  msgList [] = []
  msgList (Message msg:tl) = "  " ++ show msg ++ ",\n"

joinErrors :: PackError -> PackError -> PackError
joinErrors (el@(PackError pl dl)) (er@(PackError pr dr))
  | pl < pr || null dl = er
  | pr < pl || null dr = el
  | otherwise = PackError pl (union dl dr)

nullError :: Derivs d => d -> PackError
nullError dvs = PackError (dvPos dvs) []

-- Utils function
isExpect :: PackEDesc -> Bool
isExpect (Expected _) = True
isExpect _ = False

isUnexpect :: PackEDesc -> Bool
isUnexpect (Unexpected _) = True
isUnexpect _ = False

isMessage :: PackEDesc -> Bool
isMessage (Message _) = True
isMessage _ = False

getExpects :: [PackEDesc] -> [PackEDesc] 
getExpects = filter isExpect

getUnexpects :: [PackEDesc] -> [PackEDesc] 
getUnexpects = filter isUnexpect

getMessages:: [PackEDesc] -> [PackEDesc] 
getMessages = filter isMessage
