{-# LANGUAGE MultiParamTypeClasses #-}
module Packrat.Stateful (
  Stateful(..),
  putState,
  modState,
  getState
) where

import Packrat.Prim
import Packrat.Positions (PackPos, nextPos)

class Derivs d => Stateful d s where
  sdvState :: d -> s
  sdvSetup :: d -> PackPos -> s -> String -> d

putStateI :: Stateful d s => s -> d -> Result d ()
putStateI state d = Parsed () d' (nullError d)
  where pos = dvPos d
        input = dvInput d
        setup = sdvSetup d
        d' = setup pos state input

putState :: Stateful d s => s -> Parser d ()
putState s = Parser (putStateI s)

modState :: Stateful d s => (s -> s) -> Parser d ()
modState f = Parser p
  where p d = putStateI (f (sdvState d)) d

getState :: Stateful d s => Parser d s
getState = Parser $ \d -> Parsed (sdvState d) d (nullError d)
