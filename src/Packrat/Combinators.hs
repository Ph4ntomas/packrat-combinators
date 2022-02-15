module Packrat.Combinators (
  neg
  , satisfy
  , peek
  , notFollowedBy
  , between
  , sepBy1
  , sepBy
  , endBy1
  , endBy
  , sepEndBy1
  , sepEndBy
  , chainl1
  , chainl
  , chainr1
  , chainr
  , choice
) where

import Control.Applicative
import Data.Maybe

import Packrat.Prim
import Packrat.Errors
import Packrat.Positions

-- | Negate a parser, making it fail if the parser worked.
neg :: Derivs d => Parser d v -> Parser d ()
neg (Parser p) = Parser try where
  try dvs = case p dvs of
    Parsed v rem err -> NoParse (unexpError dvs unexpected)
      where unexpected = take (fromJust $ distance (dvPos dvs) (dvPos rem)) (dvInput dvs) 
    NoParse e -> Parsed () dvs (nullError dvs)

-- | Runs a parser and check if its result satisfy a given predicate
satisfy :: Derivs d => Parser d v -> (v -> Bool) -> Parser d v
satisfy (Parser p) pred = Parser check where
  check dvs = case p dvs of
    res@(Parsed v rem err)
      | pred v -> res
      | otherwise -> NoParse (nullError dvs)
    NoParse e -> NoParse e

-- | Run a parser, returning it's input if it worked, without consuming any input
peek :: Derivs d => Parser d v -> Parser d v
peek (Parser p) = Parser try where
  try dvs = case p dvs of
    Parsed v rem err -> Parsed v dvs (nullError dvs)
    err -> err

-- | Ensure that a parser is not followed by another one.
notFollowedBy :: Derivs d => Parser d v -> Parser d a -> Parser d v
notFollowedBy v d = v <* neg d

between :: Derivs d => Parser d v -> Parser d a -> Parser d v
between v d = d *> v <* d

-- Iterative combinator 
-- These can break memoization.
sepBy1 :: Derivs d => Parser d v -> Parser d sep -> Parser d [v]
sepBy1 p psep = (:) <$> p <*> many (psep *> p)

sepBy :: Derivs d => Parser d v -> Parser d sep -> Parser d [v]
sepBy p psep = sepBy1 p psep <|> pure []

endBy1 :: Derivs d => Parser d v -> Parser d vend -> Parser d [v]
endBy1 p pend = some (p <* pend)

endBy :: Derivs d => Parser d v -> Parser d vend -> Parser d [v]
endBy p pend = many (p <* pend)

sepEndBy1 :: Derivs d => Parser d v -> Parser d sep -> Parser d [v]
sepEndBy1 p psep = sepBy1 p psep <* optional psep

sepEndBy :: Derivs d => Parser d v -> Parser d sep -> Parser d [v]
sepEndBy p psep = sepBy p psep <* optional psep

chainl1 :: Derivs d => Parser d v -> Parser d (v -> v -> v) -> Parser d v
chainl1 p psep = p >>= chain where
  chain v = ((psep <*> pure v <*> p) >>= chain) <|> return v

chainl :: Derivs d => Parser d v -> Parser d (v -> v -> v) -> v -> Parser d v
chainl p psep v = chainl1 p psep <|> return v

chainr1 :: Derivs d => Parser d v -> Parser d (v -> v -> v) -> Parser d v
chainr1 p psep = (psep <*> p <*> chainr1 p psep) <|> p

chainr :: Derivs d => Parser d v -> Parser d (v -> v -> v) -> v -> Parser d v
chainr p psep v = chainr1 p psep <|> return v

choice :: Derivs d => [Parser d v] -> Parser d v
choice [p] = p
choice (p:nxt) = p <|> choice nxt
