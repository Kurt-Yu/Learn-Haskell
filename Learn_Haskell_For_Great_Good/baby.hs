removeNonUpper :: [Char] -> [Char]
removeNonUpper st = [c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky Number"
lucky x = "Sorry, you're out of luck, pal!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

head' :: [a] -> a
head' [] = error "Can't call head on a empty list, dummy!"
head' (x:_) = x

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs 

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty string, whoops"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You are supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"

bmiTell' :: (RealFloat a) => a -> a -> String  
bmiTell' weight height 
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise = "You're a whale, congratulations!"  

max' :: (Ord a) => a -> a -> a
max' a b
    | a < b = b
    | otherwise = a

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b = GT
    | a == b = EQ
    | otherwise = LT

bmiTell'' :: (RealFloat a) => a -> a -> String
bmiTell'' weight height
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Piffy, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratualtions!"
    where bmi = weight / height ^ 2

-- Notice that all the names are aligned at a single column.
-- If we don't align them nice and proper, Haskell gets confused 
-- because then it doesn't know they're all part of the same block
bmiTellWithWhere :: (RealFloat a) => a -> a -> String  
bmiTellWithWhere weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0 

-- The where statement could also written like this:
{- 
where bmi = weight / height ^ 2  
      (skinny, normal, fat) = (18.5, 25.0, 30.0)  
-}

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

-- let expressions: 
-- Notice that the names are also aligned in a single column
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

-- the difference between where and let is that let is an expression
{-
4 * (let a = 9 in a + 1) + 2

[let square x = x * x in (square 5, square 3, sqaure 2)]
-}

-- If we want to bind serveral variables inline, seperate with semicolons
{- 
(let a = 100; b = 200; c = 300; in a * b * c, let foo = "Hey "; bar = "there!" in foo ++ bar)
-}

-- put let bindings inside list comprehensions:
calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]
 
-- Case expressions:
head'' :: [a] -> a
head'' xs = case xs of [] -> error "No head of empty lists!"
                       (x:_) -> x

describeList :: [a] -> String
describeList xs = "This list is " ++ case xs of [] -> "empty"
                                                [x] -> "a singleton list"
                                                xs -> "a longer list."

describeList' :: [a] -> String
describeList' xs = "This list is " ++ what xs
    where what [] = "empty"
          what [x] = "a singleton list"
          what xs = "a longer list"