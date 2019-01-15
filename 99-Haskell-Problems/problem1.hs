{-
Find the last element of a list.
-}

myLast :: [a] -> a
myLast [] = error "No end for empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

myLast' = head . reverse