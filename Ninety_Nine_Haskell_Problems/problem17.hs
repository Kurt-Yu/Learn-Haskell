{-
Split a list into two parts; 
the length of the first part is given
-}

-- method 1:
split :: [a] -> Int -> ([a], [a])
split xs 0 = ([], xs)
split (x:xs) k =
    let helper = split xs (k - 1) 
    in (x : fst helper, snd helper)  

-- method 2: using take and drop:
split' :: [a] -> Int -> ([a], [a])
split' xs k = (take k xs, drop k xs)

-- method 3: just using splitAt:
split'' :: [a] -> Int -> ([a], [a])
split'' = flip splitAt