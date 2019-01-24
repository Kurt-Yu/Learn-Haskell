import System.Random

{-
random :: (RandomGen g, Random a) => g -> (a, g)
-}

-- make a function that simulates tossing a coin three times:
threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in (firstCoin, secondCoin, thirdCoin)


-- there is a function called `randoms` that takes a generator
-- and returns an infinite sequence of values based on that generator
-- implement our own `randoms`:
randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value : randoms' newGen


-- a simple program that generates a raondom string:
main = do
    gen <- getStdGen
    let randomChars = randomRs ('a', 'z') gen
        (first20, rest) = splitAt 20 randomChars
        (second20, _) = splitAt 20 rest
    putStrLn first20
    putStrLn second20
