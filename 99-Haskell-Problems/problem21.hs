{-
Insert an element at a given position into a list
-}

-- method 1:
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs 1 = x:xs
insertAt x (y:xs) n = y : insertAt x xs (n - 1)

-- method 2:
insertAt' :: a -> [a] -> Int -> [a]
insertAt' x xs n = a ++ (x:b)
    where (a, b) = splitAt (n - 1) xs 