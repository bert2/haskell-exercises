import Data.Char

toDigits :: Integer -> [Integer]
toDigits n
    | n < 1     = []
    | otherwise = map (toInteger . digitToInt) (show n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []       = []
doubleEveryOther [x]      = [x]
doubleEveryOther (x:y:zs) = x : 2*y : doubleEveryOther zs

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

validate :: Integer -> Bool
validate n = (checksum n) `mod` 10 == 0
    where checksum = sumDigits . doubleEveryOther . reverse . toDigits