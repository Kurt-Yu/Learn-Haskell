{-
Run-length encoding of a list. 
Use the result of problem P09 to implement the so-called run-length 
encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) 
where N is the number of duplicates of the element E
-}
import Data.List

-- method 1:
pack :: (Eq a) => [a] -> [[a]]
pack (x:xs) = let (first, rest) = span (== x) xs
                in (x:first) : pack rest
pack [] = []

helper :: [[a]] -> [(Int, a)]
helper [] = []
helper (x:xs) = (length x, head x) : helper xs 

encode :: (Eq a) => [a] -> [(Int, a)]
encode = helper . pack

-- method 2:
encode' :: (Eq a) => [a] -> [(Int, a)]
encode' xs = map (\x -> (length x, head x)) (group xs)

-- method 3: using list compression:
encode'' xs = [(length x, head x) | x <- group xs]