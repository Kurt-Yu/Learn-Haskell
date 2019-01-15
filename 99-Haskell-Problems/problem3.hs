{-
Find the K'th element of a list. The first element in the list is number 1.
-}

{- Notice how infix operator "!!" defined in haskell:
(!!) :: [a] -> Int -> a
(x:_) !! 0 = x
(_:xs) !! n = xs !! (n - 1)
-}

-- method 1: using infix operator:
elementAt :: [a] -> Int -> a
elementAt xs n = xs !! (n - 1)

-- method 2: without infix operator:
elementAt' [] _ = error "Index out of bound"
elementAt' (x:_) 1 = x
elementAt' (_:xs) n = 
    if n < 1 
    then error "Index out of bound" 
    else elementAt' xs (n - 1)

-- the first two methods does not work with infinte lists
-- method 3: using build in functions
elementAt'' xs n 
    | length xs < n = error "Index out of bound"
    | otherwise = fst . last $ zip xs [1..n]