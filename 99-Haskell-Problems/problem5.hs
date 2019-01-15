{-
Reverse a list
-}

-- method 1: bad solution, since "++" operator takes long time
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- method 2: using an accumulator:
myReverse' xs = rev xs []
    where rev [] acc = acc
          rev (x:xs) acc = rev xs (x:acc)

-- method 3: using flip function (which is the source code uses)
{-
flip :: (a -> b -> c) -> b -> a -> c
it evaluates the function flipping the order of arguments

Example 1: flip (/) 1 2
Output:     2.0

Example 2: flip (>) 3 5
Output:    True
-}
myReverse'' :: [a] -> [a]
myReverse'' = foldl (flip (:)) []

-- method 4: another foldl solution:
myReverse''' :: [a] -> [a]
myReverse''' = foldl (\a x -> x:a) []