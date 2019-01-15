import Control.Monad
import Data.Char

main = forever $ do
    contents <- getContents
    putStrLn (map toUpper contents)