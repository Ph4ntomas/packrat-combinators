module Example where

import Control.Applicative

import Packrat.Positions
import Packrat.Prim
import Packrat.Errors
import Packrat.Combinators
import Packrat.Char

data ExampleDerivs = ExampleDerivs {
                  edvAabb :: Result ExampleDerivs [Char],

                  edvA :: Result ExampleDerivs Char,
                  edvB :: Result ExampleDerivs Char,

                  edvPos :: PackPos,
                  edvChar :: Result ExampleDerivs Char
} 

instance Derivs ExampleDerivs where
  dvPos = edvPos
  dvChar = edvChar

eval :: String -> String -> String
eval file str = case sub (parse (PackPos file 1 1) str) of
    Parsed v _ err -> v ++ "\n" ++ show err
    NoParse err -> show err
  where Parser sub = Parser pAabb

parse :: PackPos -> String -> ExampleDerivs
parse pos s = d where
  d = ExampleDerivs aabb a b pos char
  aabb = pAabb d
  a = pA d
  b = pB d
  char = case s of
    [] -> NoParse (eofError d)
    (c:rem) -> Parsed c (parse (nextpos pos c) rem) (nullError d)

pAabb :: ExampleDerivs -> Result ExampleDerivs [Char]
Parser pAabb = some (Parser edvA) <> some (Parser edvB) <* eof

pA :: ExampleDerivs -> Result ExampleDerivs Char
Parser pA = char 'a'

pB :: ExampleDerivs -> Result ExampleDerivs Char
Parser pB = char 'b'


