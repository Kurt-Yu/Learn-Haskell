{-
Find the last but one element of a list.
-}

myButLast :: [a] -> a
myButLast [] = error "Empty List"
myButLast [x] = error "Singleton List"
myButLast (x:xs) = if length xs == 1 then x else myButLast xs

myButLast' = last . init

myButLast'' x = reverse x !! 1