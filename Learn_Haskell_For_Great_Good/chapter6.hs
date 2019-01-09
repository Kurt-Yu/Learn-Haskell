-- Higher Order Functions
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- curried function
compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred x = compare 100 x

compareWithHundred' :: (Num a, Ord a) => a -> Ordering
compareWithHundred' = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperLetter :: Char -> Bool
isUpperLetter = (`elem` ['A'..'Z'])

-- Functions can take functions as parameters and also return functions
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g where g x y = f y x

flip'' :: (a -> b -> c) -> (b -> a -> c)
flip'' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) 
    | f x = x : filter' f xs
    | otherwise = filter' f xs
-- filter' f (x:xs) = if f x then x : filter' f xs else filter' f xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort (filter (<= x) xs)
        biggerSorted = quicksort (filter (> x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

-- find the largest number under 100,000 that's divisible by 3829
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [10000,9999..])
    where p x = x `mod` 3829 == 0

-- find the sum of all odd squares that are smaller than 10,000
{-
"takeWhile" function:
It takes a predicate and a list and then goes from the beginning of the list 
and returns its elements while the predicate holds true

sum (takeWhile (< 10000) (filter odd (map (^) [1..])))

sum (takeWhile (< 10000) [n | n <- [1..], odd (n ^ 2)])
-}


-- dealing with Collatz sequences:
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | odd n = n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

-- lambda:
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

filp'' :: (a -> b -> c) -> (b -> a -> c)
filp'' f = \x y -> f y x

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

-- Function application:
{- $ 
($) :: (a -> b) -> a -> b  
f $ x = f x 
-}

-- Function composition
{-
.
(.) :: (b -> c) -> (a -> b) -> a -> c  
f . g = \x -> f (g x)  
-}