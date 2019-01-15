{-
Eliminate consecutive duplicates of list elements
-}
import Data.List

-- method 1: using foldr
compress :: (Eq a) => [a] -> [a]
compress xs = foldr (\x acc -> if x == (head acc) then acc else x:acc) [last xs] xs

-- method 2: using group function:
-- We simply group equal values together, then take the head of each.
{-
group :: Eq a => [a] -> [[a]]

The group function takes a list and returns a list of lists 
such that the concatenation of the result is equal to the argument. 
Moreover, each sublist in the result contains only equal elements.

Example: group "Mississippi"
Output : ["M","i","ss","i","ss","i","pp","i"]
-}
compress' :: (Eq a) => [a] -> [a]
compress' = map head . group

-- method 3: using pattern matching
compress'' :: (Eq a) => [a] -> [a]
compress'' (x:ys@(y:_))
    | x == y = compress'' ys
    | otherwise = x : compress'' ys
compress'' ys = ys

-- method 4: another approach using dropWhile
{-
dropWhile :: (a -> Bool) -> [a] -> [a]

creates a list from another one, it inspects the original list 
and takes from it its elements from the moment 
when the condition fails for the first time till the end of the list

Input: dropWhile (<3) [1,2,3,4,5]
Output: [3,4,5]

Input: dropWhile even [2,4,6,7,9,11,12,13,14]
Output: [7,9,11,12,13,14]
-}
compress4 :: (Eq a) => [a] -> [a]
compress4 [] = []
compress4 (x:xs) = x : (compress4 $ dropWhile (== x) xs)