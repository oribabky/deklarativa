--sums up numerical indices in a list
sumList :: Num t => [t] -> t
sumList [] = 0
sumList (x : xs) = x + sumList xs

