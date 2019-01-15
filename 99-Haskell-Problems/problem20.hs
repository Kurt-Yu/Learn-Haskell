{-
Remove the K'th element from a list
-}

-- method 1:
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (last first, init first ++ second)
    where (first, second) = splitAt n xs

-- method 2:
removeAt' n xs = (xs !! (n - 1), take (n - 1) xs ++ drop n xs)