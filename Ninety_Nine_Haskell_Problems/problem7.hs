{-
Flatten a nested list structure
-}

-- we need to define a new data type:
data NestedList a = Elem a | List [NestedList a]

-- method 1:
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

-- method 2: using concatMap
{-
concatMap :: (a -> [b]) -> [a] -> [b]

creates a list from a list generating function 
by application of this function on all elements in a list 
passed as the second argument

Example 1: concatMap (enumFromTo 1) [1,3,5]
Output :   [1,1,2,3,1,2,3,4,5]

Example 2: concatMap (\x -> [(x,x+2,x/2)]) [1,3,5]
Output :   [(1.0,3.0,0.5),(3.0,5.0,1.5),(5.0,7.0,2.5)]
-}
flatten' :: NestedList a -> [a]
flatten' (Elem x) = [x]
flatten' (List x) = concatMap flatten' x
