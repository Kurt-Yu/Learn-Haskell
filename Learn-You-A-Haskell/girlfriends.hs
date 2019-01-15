import System.IO

main = do
    handle <- openFile "girlfriends.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

-- another way of doing it is to use `withFile`
-- withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
{-
main = do
    withFile "girlfriends.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)
-}

-- make our own `withFile` functions:
withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result

-- `readFile`:
{-
import System.IO

main = do
    contents <- readFile "girlfriends.txt"
    putStr contents
-}

-- `writeFile`:
-- it rewrite the file is the file already exists
{-
import System.IO

main = do
    contents <- readFile "girlfriends.txt"
    writeFile "girlfriendscaps.txt" (map toUpper contents)
-}

-- `appendFile`:
{-
import System.IO

main = do
    todoItem <- getLines
    appendFile "todo.txt" (todoItem ++ "\n")
-}
