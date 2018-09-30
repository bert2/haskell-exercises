module Golf where

import Data.List

skip :: Integral a => a -> [b] -> [b]
skip n l = [x | (i,x) <- zip [1..] l, i `mod` n == 0]

skips :: [a] -> [[a]]
skips l = [skip n l | n <- [1..length l]]

localMaxima :: [Integer] -> [Integer]
localMaxima xs = [x | (l,x,r) <- win3 xs , l < x && x > r]

win3 :: [a] -> [(a,a,a)]
win3 (l:x:r:xs) = (l,x,r) : win3 (x:r:xs)
win3 _          = []

win :: Int -> [a] -> [[a]]
win n = map (take n) . filter minLen . tails
    where minLen = (>=n) . length

histoBins :: [Integer]
histoBins = [0..9]

histogram :: [Integer] -> String
histogram xs = bars ++ sep ++ "\n" ++ labels ++ "\n"
    where bars = unlines $ histoLines $ histoCount histoBins xs
          sep = replicate (length histoBins) '='
          labels = map (head . show) histoBins

histoCount :: [Integer] -> [Integer] -> [Int]
histoCount bins xs = [count b xs | b <- bins]
    where count b = length . filter (==b)

histoLines :: (Num a, Ord a, Enum a) => [a] -> [String]
histoLines xs = reverse [histoLine n xs | n <- [1..maximum xs]]

histoLine :: Ord a => a -> [a] -> String
histoLine n = map (\x -> if x >= n then '*' else ' ')
