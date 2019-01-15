{-
Duplicate the elements of a list
-}

-- method 1:
repli :: (Eq a) => [a] -> [a]
repli = concatMap (\x -> replicate 2 x)
-- or: concatMap (replicate 2)
-- or: concatMap (\x -> [x, x])

--method 2:
repli' :: (Eq a) => [a] -> [a]
repli' = foldr (\x acc -> x:x:acc) []

-- method 3:
repli'' :: (Eq a) => [a] -> [a]
repli'' [] = []
repli'' (x:xs) = x:x:repli'' xs