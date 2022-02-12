module Packrat.Errors (
  module Packrat.Internal.Errors,
  (<?>)
  , eofError
  , expError
  , expErrorAt
  , internalError
  , internalErrorAt
  , msgError
  , msgErrorAt
  , nullError
  , unexpError
  , unexpErrorAt
  , joinErrors
) where

import Data.List (union)

import Packrat.Internal.Derivs
import Packrat.Internal.Errors

import Packrat.Positions

infix 0 <?>
(<?>) :: Derivs d => Parser d v -> String -> Parser d v
(Parser p) <?> edesc = Parser tryparse where
  tryparse dvs = case p dvs of
    Parsed v rem err -> Parsed v rem (filtErr dvs err)
    NoParse err -> NoParse (filtErr dvs err)
  filtErr dvs err@(PackError pos descs)
    | pos > dvPos dvs = err
    | otherwise = expError dvs edesc

eofError :: Derivs d => d -> PackError
eofError dvs = unexpError dvs "end of file"

expError :: Derivs d => d -> String -> PackError
expError dvs msg = PackError (dvPos dvs) [Expected msg]

expErrorAt :: PackPos -> String -> PackError
expErrorAt pos msg = PackError pos [Expected msg]

internalError :: Derivs d => d -> String -> PackError
internalError dvs msg = PackError (dvPos dvs) [Internal msg]

internalErrorAt :: PackPos -> String -> PackError
internalErrorAt pos msg = PackError pos [Internal msg]

msgError :: Derivs d => d -> String -> PackError
msgError dvs msg = PackError (dvPos dvs) [Message msg]

msgErrorAt :: PackPos -> String -> PackError
msgErrorAt pos msg = PackError pos [Message msg]

nullError :: Derivs d => d -> PackError
nullError dvs = PackError (dvPos dvs) []

unexpError :: Derivs d => d -> String -> PackError
unexpError dvs msg = PackError (dvPos dvs) [Unexpected msg]

unexpErrorAt :: PackPos -> String -> PackError
unexpErrorAt pos msg = PackError pos [Unexpected msg]

joinErrors :: PackError -> PackError -> PackError
joinErrors el@(PackError pl dl) er@(PackError pr dr)
  | pl < pr || null dl = er
  | pr < pl || null dr = el
  | otherwise = PackError pl (dl `union` dr)
