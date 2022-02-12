module Packrat.Positions where

data PackPos = PackPos {
              filename :: String,
              line :: Int,
              column :: Int,
              offset :: Int,
              tab_length :: Int
}

newPos :: String -> PackPos
newPos file = PackPos file 1 1 0 4

newPosTab :: String -> Int -> PackPos
newPosTab file = PackPos file 1 1 0

nextPos :: PackPos -> Char -> PackPos
nextPos pos@(PackPos _ l c o _) '\n' = pos {line=l + 1, offset=o + 1}
nextPos pos@(PackPos _ _ c o le) '\t' = pos {column = c + le, offset=o + 1}
nextPos pos@(PackPos _ _ c o _) _ = pos {column = c + 1, offset=o + 1}

distance :: PackPos -> PackPos -> Maybe Int
distance (PackPos f _ _ o _) (PackPos f' _ _ o' _)  | f == f' = Just $ abs $ o - o'
                                                    | otherwise = Nothing

instance Eq PackPos where
  (PackPos fl ll cl _ _) == (PackPos fr lr cr _ _) =
    fl == fr && ll == lr && cl == cr

instance Ord PackPos where
  (PackPos fl ll cl _ _) <= (PackPos fr lr cr _ _) =
    (ll < lr) || (ll == lr && cl <= cr)

instance Show PackPos where
  show (PackPos f l c _ _) = show f ++ ":" ++ show l ++ ":" ++ show c ++ ": "
