{-
Find out whether a list is a palindrome.
-}

-- method 1: just reverse the list and then compare
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- method 2: pattern matching
isPalindrome' [] = True
isPalindrome' [x] = True
isPalindrome' xs = (head xs) == (last xs) && (isPalindrome' $ init $ tail xs)


-- method 3: un-reveal the first method
isPalindrome'' xs = foldl (\acc (a, b) -> if a == b then acc else False) True input
    where input = zip xs (reverse xs)