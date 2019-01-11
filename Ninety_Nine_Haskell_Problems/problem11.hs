{-
Modify the result of problem 10 in such a way that 
if an element has no duplicates it is simply copied into the result list. 
Only elements with duplicates are transferred as (N E) lists
-}
import Data.List

data ListItem a = Single a | Multiple Int a

-- method 1:
encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified xs = map (\x -> if length x == 1 then Single (head x) else Multiple (length x) (head x)) (group xs)

-- method 2:
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) (group xs)

encodeModified' :: (Eq a) => [a] -> [ListItem a]
encodeModified' = map encodeHelper . encode
    where encodeHelper (1, x) = Single x
          encodeHelper (n, x) = Multiple n x
