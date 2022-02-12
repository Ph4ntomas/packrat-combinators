module Packrat.Utils (
  position
) where 

import Packrat.Prim
import Packrat.Errors
import Packrat.Positions (PackPos)

position :: Derivs d => Parser d PackPos
position = Parser (\d -> Parsed (dvPos d) d (nullError d))
