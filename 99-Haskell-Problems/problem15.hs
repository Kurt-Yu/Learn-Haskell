{-
Replicate the elements of a list a given number of times
-}

-- method 1: using replicate
repli :: (Eq a) => [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

-- method 2: point-free style
repli' :: (Eq a) => [a] -> Int -> [a]
repli' = flip $ concatMap . replicate

-- method 3: without using replicate:
repli'' :: (Eq a) => [a] -> Int -> [a]
repli'' xs n = concatMap (take n . repeat) xs