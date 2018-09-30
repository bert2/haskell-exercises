sumtorial :: Integer -> Integer
sumtorial n
    | n <= 0    = 0
    | otherwise = n + sumtorial (n - 1)

hailstone :: Integer -> Integer
hailstone n
    | n `mod` 2 == 0 = n `div` 2
    | otherwise      = 3 * n + 1

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

sumEveryTwo []       = []
sumEveryTwo (x:[])   = [x]
sumEveryTwo (x:y:zs) = (x + y) : sumEveryTwo zs

data Fruit = Apple | Orange deriving Show
whichFruit f =
    case f of
        "apple" | False -> Apple
        "orange" -> Orange
