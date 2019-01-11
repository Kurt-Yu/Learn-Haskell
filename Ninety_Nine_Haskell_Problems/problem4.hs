{-
Find the number of elements of a list
-}

-- method 1:
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- method 2: using an accumulator:
myLength' xs = acc xs 0
    where acc [] n = n
          acc (_:xs) n = acc xs (n + 1)

-- method 3: mapping all elements to 1
myLength'' = sum . map (\_ -> 1)

-- method4: using fold
myLength''' :: [a] -> Int
myLength''' = foldl (\n _ -> n + 1) 0
-- or: mylength = foldr (\_ n -> n + 1) 0
-- or: myLength = foldr (\_ -> (+1)) 0