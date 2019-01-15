{-
Given two indices, i and k, the slice is the list 
containing the elements between the i'th and k'th element of the original list 
(both limits included). Start counting the elements with 1
-}

-- method 1:
slice :: [a] -> Int -> Int -> [a]
slice xs n m = take (m - n + 1) $ drop (n - 1) xs

-- method 2: using zip and list comprehension
slice' :: [a] -> Int -> Int -> [a]
slice' xs n m = [b | (a, b) <- zip [1..] xs, a >= n && a <= m]