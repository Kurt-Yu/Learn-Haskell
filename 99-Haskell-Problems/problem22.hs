{-
Create a list containing all integers within a given range
-}

-- method 1:
range :: Int -> Int -> [Int]
range a b = drop (a - 1) (take b [1..])

-- method 2:
range' :: Int -> Int -> [Int]
range' a b = [a..b]

-- method 3:
range'' :: (Enum a) => a -> a -> [a]
range'' = enumFromTo