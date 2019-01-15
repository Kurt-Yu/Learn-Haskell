{-
Drop every N'th element from a list
-}

-- method 1:
dropEvery :: (Eq a) => [a] -> Int -> [a]
dropEvery xs k = map snd $ filter (\(a, b) -> a `mod` k /= 0) ys
    where ys = zip [1..] xs

-- method 2: using list comprehension
dropEvery' :: (Eq a) => [a] -> Int -> [a]
dropEvery' xs k = [b | (a, b) <- zip [1..] xs, a `mod` k /= 0]