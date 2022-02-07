module Packrat.Positions where

data PackPos = PackPos {
              filename :: String,
              line :: Int,
              column :: Int,
              tab_length :: Int
}

newPos :: String -> PackPos
newPos file = PackPos file 1 1 4

newPosTab :: String -> Int -> PackPos
newPosTab file = PackPos file 1 1 

nextPos :: PackPos -> Char -> PackPos
nextPos pos@(PackPos _ l c _) '\n' = pos {line=l + 1}
nextPos pos@(PackPos _ _ c le) '\t' = pos {column = c + le}
nextPos pos@(PackPos _ _ c _) _ = pos {column = c + 1}

instance Eq PackPos where
  (PackPos fl ll cl _) == (PackPos fr lr cr _) =
    fl == fr && ll == lr && cl == cr

instance Ord PackPos where
  (PackPos fl ll cl _) <= (PackPos fr lr cr _) =
    (ll < lr) || (ll == lr && cl <= cr)

instance Show PackPos where
  show (PackPos f l c _) = show f ++ ":" ++ show l ++ ":" ++ show c ++ ": "
