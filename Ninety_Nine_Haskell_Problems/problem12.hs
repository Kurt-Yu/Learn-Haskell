{-
Given a run-length code list generated as specified in problem 11. 
Construct its uncompressed version.
-}

data ListItem a = Single a | Multiple Int a

-- method 1:
decodeModified :: (Eq a) => [ListItem a] -> [a]
decodeModified [] = []
decodeModified ((Single x):xs) = x : decodeModified xs
decodeModified ((Multiple n x):xs) =
    if n > 0 then x : decodeModified ((Multiple (n - 1) x):xs)
    else decodeModified xs

-- method 2: using concatMap and replicate
decodeModified' :: [ListItem a] -> [a]
decodeModified' = concatMap decodeHelper
    where decodeHelper (Single x) = [x]
          decodeHelper (Multiple n x) = replicate n x