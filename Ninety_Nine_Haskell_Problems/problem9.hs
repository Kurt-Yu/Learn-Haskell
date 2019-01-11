{-
Pack consecutive duplicates of list elements into sublists.
If a list contains repeated elements they should be placed in separate sublists
-}

-- method 1: using takeWhile and dropWhile
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack xs = (takeWhile (== head xs) xs) : (pack $ dropWhile (== head xs) xs)

-- method 2: using span, (the defualt implementation for group in haskell library)
{-
span :: (a -> Bool) -> [a] -> ([a], [a])

span, applied to a predicate p and a list xs, 
returns a tuple where first element is longest prefix 
(possibly empty) of xs of elements that satisfy p and 
second element is the remainder of the list

span (< 3) [1,2,3,4,1,2,3,4] == ([1,2],[3,4,1,2,3,4])

span p xs is equivalent to (takeWhile p xs, dropWhile p xs)
-}
pack' :: (Eq a) => [a] -> [[a]]
pack' (x:xs) = let (first, rest) = span (== x) xs
                in (x:first) : pack' rest
pack' [] = []