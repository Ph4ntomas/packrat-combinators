module Packrat.Positions where

data PackPos = PackPos {
              filename :: String,
              line :: Int,
              column :: Int
}

nextpos :: PackPos -> Char -> PackPos
nextpos (PackPos f l c) '\n' = PackPos f (l + 1) c
nextpos (PackPos f l c) '\t' = PackPos f l (c + 4)
nextpos (PackPos f l c) _ = PackPos f l (c + 1)

instance Eq PackPos where
  (PackPos fl ll cl) == (PackPos fr lr cr) =
    fl == fr && ll == lr && cl == cl

instance Ord PackPos where
  (PackPos fl ll cl) <= (PackPos fr lr cr) =
    (ll < lr) || (ll <= lr && cl <= cr)
