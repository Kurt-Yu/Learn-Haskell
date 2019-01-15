{-
ghci> :t putStrLn
putStrLn :: String -> IO ()
putStrLn takes a string and returns an I/O action that has the result type of ()
-}

import Data.Char
import Control.Monad

{-
main = do
    putStrLn "What's your first name?"
    firstname <- getLine
    putStrLn "What's your last name?"
    lastname <- getLine
    let biggerFirstName = map toUpper firstname
        biggerLastName = map toUpper lastname -- the in part is not necessary
    putStrLn $ "hey " ++ biggerFirstName ++ " " ++ biggerLastName ++ ", how are you?"
-}

-- getLine :: IO String
-- getLine is an I/O action that contains a result type of string

-- make a program that continuously reads a line and prints out the same line with the words reverse
{-
main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main
-}

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

-- return does not mean to end the program
-- it just takes a value and wraps it up into an I/O action
-- while "<-" takes an I/O action and extract the value from it and then bind it to some variable

-- putStr doesn't jump to a new line after printing out the string
-- putStr :: String -> IO ()
{-
main = do putStr "Hey, "
          putStr "I'm "
          putStr "Andy!"

-- result is: Hey, I'm Andy
-}

-- putChar takes a character and returns an I/O action that will print it out to the terminal
{-
main = do
    putChar 't'
    putChar 'e'
    putChar 'h'

-- result: teh
-}

-- putStr is actually defined recursively with the help of putChar
putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do 
    putChar x
    putStr' xs

-- `print` takes a value of any type that's  an instance of `Show`
-- calls `show` with that value to stringify it and then outputs that string to the terminal
-- bascially, it's just `putStrLn . show`
{-
main = do
    print True
    print 2
    print "haha"
    print 3.2
    print [3,4,3]
-}

-- every I/O action are performed only when they all into `main`
-- when we try to evaluate then in the GHCI prompt, when we type a vlaue and hit enter
-- ghci actually calls the `print` function with that value

-- `getChar` is an I/O action that reads a character from the input.
-- getChar :: IO Char
{-
main = do
    c <- getChar
    if c /= ' '
        then do 
            putChar c
            main
        else return ()
-}


-- the when function is found in Control.Monad
-- it takes a boolean value and an I/O action
-- if that boolean is True, it returns the same I/O action that we supplied
-- if that boolean is False, it returns `return ()`

{-
main = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main
-}
-- it is useful to encapsulating the `if something then do some I/O action else return ()` pattern


--`sequence` takes a list of I/O actions and perform those actions in order
-- the result is a list of the result of all the I/O action
-- sequence :: [IO a] -> IO [a]

{-
main = do
    a <- getLine
    b <- getLine 
    c <- getLine
    print [a, b, c]
-}
-- is the same as:
{-
main = do
    rs <- sequence [getLine, getLine, getLine]
    print rs
-}


-- `mapM` and `mapM_` takes a function and a list,
-- maps the function over the list and then sequence it
-- `mapM_` throws the result away


-- `forever` takes an I/O action and returns an I/O action that just repeats the I/O action it got forever

{-
main = forever $ do
    putStr "Give me some input:"
    l <- getLine
    putStrLn $ map toUpper l
-}
{-
main = do
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
        color <- getLine
        return color)
    putStrLn "The color that you associated with 1, 2, 3 and 4 are: "
    mapM putStrLn colors
-}

-- write a program that takes some input and onlu ouputs those lines are shorter than 10 characters
{-
main = do
    contents <- getContents
    putStr (shortLinesOnly contents)

shortLinesOnly :: String -> String
shortLinesOnly input = 
    let allLines = lines input
        shortLines = filter (\line -> length line < 10) allLines
        result = unlines shortLines
    in result
-}

-- wirte a program that continuouslu reads a line and tell us if the line is palindrome or not
respondPalindromes contents = unlines $ map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") (lines contents)
    where isPalindrome xs = xs == reverse xs

main = interact respondPalindromes