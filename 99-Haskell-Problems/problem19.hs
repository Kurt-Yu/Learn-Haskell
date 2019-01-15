{-
Rotate a list N places to the left.
-}

-- method 1:
rotate :: [a] -> Int -> [a]
rotate xs n = b ++ a
    where (a, b) = splitAt ((length xs + n) `mod` (length xs)) xs

-- method 2:
rotate' xs n
    | n >= 0 = drop n xs ++ take n xs
    | otherwise = drop len xs ++ take len xs
        where len = n + length xs

-- method 3:
rotate'' xs n = drop nn xs ++ take nn xs
    where nn = n `mod` length xs