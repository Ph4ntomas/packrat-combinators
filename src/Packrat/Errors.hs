module Packrat.Errors where

import Packrat.Prim
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

expError :: Derivs d => d -> String -> PackError
expError dvs msg = PackError (dvPos dvs) [Expected msg]

expErrorAt :: PackPos -> String -> PackError
expErrorAt pos msg = PackError pos [Expected msg]

unexpError :: Derivs d => d -> String -> PackError
unexpError dvs msg = PackError (dvPos dvs) [Unexpected msg]

unexpErrorAt :: PackPos -> String -> PackError
unexpErrorAt pos msg = PackError pos [Unexpected msg]

msgError :: Derivs d => d -> String -> PackError
msgError dvs msg = PackError (dvPos dvs) [Message msg]

msgErrorAt :: PackPos -> String -> PackError
msgErrorAt pos msg = PackError pos [Message msg]

eofError :: Derivs d => d -> PackError
eofError dvs = msgError dvs "end of input"
