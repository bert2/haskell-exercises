import Data.Char (digitToInt, isDigit, isSpace)
import Data.List (foldl', groupBy)
import Control.Applicative (liftA2)
import Control.Monad (liftM2)

asInt_fold :: String -> Int
asInt_fold []           = error "no digits in string"
asInt_fold ('-':'-':xs) = error "more than one sign"
asInt_fold ('-':xs)     = negate $ asInt_fold xs
asInt_fold xs           = checkOverflow $ foldl' appendDigit 0 xs
    where appendDigit n d | d == '.'  = error "not an integer"
                          | otherwise =  10 * n + digitToInt d
          checkOverflow n | show n /= xs = error "overflow"
                          | otherwise    = n

type ErrorMessage = String

asInt_either :: String -> Either ErrorMessage Int
asInt_either []           = Left "no digits in string"
asInt_either ('-':'-':xs) = Left "more than one sign"
asInt_either ('-':xs)     = negate <$> asInt_either xs
asInt_either xs           = do let ds = map digitToInt' xs
                               n <- accumulateInt ds
                               checkOverflow n
    where digitToInt' c | isDigit c = Right $ digitToInt c
                        | c == '.'  = Left "not an integer"
                        | otherwise = Left $ "not a digit " ++ show c
          accumulateInt = let appendDigit n d = 10 * n + d
                          in foldl' (liftM2 appendDigit) (Right 0)
          checkOverflow n | show n == xs = Right n
                          | otherwise    = Left "overflow"

concat' :: [[a]] -> [a]
concat' xss = foldr (++) [] xss

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _ []       = []
groupBy' p (h:xs) = let (hs, xs') = span (p h) xs
                    in (h:hs) : groupBy' p xs'

groupBy'' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy'' _ []       = []
groupBy'' p xs = let x = last xs
                     xs' = init xs
                     (_,group,groups) = foldr f (x,[x],[]) xs'
                 in group:groups
    where f y (x,group,groups) | p y x     = (y, y:group, groups      )
                               | otherwise = (y, [y]    , group:groups)

any' :: (a -> Bool) -> [a] -> Bool
any' p = foldr (\x z -> z || p x) False

cycle' :: [a] -> [a]
cycle' xs = foldr (:) (cycle' xs) xs

words' :: String -> [String]
words' xs = let (word, words) = foldr f ([],[]) xs
            in prependNonNull word words
    where f x (word,words) | isSpace x = ([], prependNonNull word words)
                           | otherwise = (x:word, words)
          prependNonNull xs xss = if null xs then xss else xs:xss

unlines' :: [String] -> String
unlines' = foldr (\x z -> x ++ "\n" ++ z) ""
