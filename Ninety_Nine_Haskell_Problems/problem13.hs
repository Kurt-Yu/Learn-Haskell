{-
Implement the so-called run-length encoding data compression method directly
-}
import Data.List

data ListItem a = Single a | Multiple Int a deriving (Show)

-- method 1:
encodeDirect :: (Eq a) => [a] -> [ListItem a]
encodeDirect xs = [if length x == 1 then Single (head x) else Multiple (length x) (head x) | x <- group xs]

-- method 2:
encode :: (Eq a) => [a] -> [(Int, a)]
encode = foldr helper []
    where helper x [] = [(1, x)]
          helper x (y@(a, b):ys)
            | x == b = (a + 1, b):ys
            | otherwise = (1, x):y:ys

encodeDirect' :: (Eq a) => [a] -> [ListItem a]
encodeDirect' = map encodeHelper . encode
    where encodeHelper (1, x) = Single x
          encodeHelper (n, x) = Multiple n x