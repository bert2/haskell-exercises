{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.List.Utils (replace)

xor :: [Bool] -> Bool
xor = odd . foldr incIf 0

xand :: [Bool] -> Bool
xand = odd . foldr (incIf . not) 0

incIf :: Enum a => Bool -> a -> a
incIf x = if x then succ else id

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z xs = foldr acc id xs z
    where acc x g = \z' -> g (f z' x)

primes :: Integer -> [Integer]
primes n = map succ . map (*2) . filter (`notElem` excluded) $ [1..n]
    where excluded = map sieve $ cartProd [1..n] [1..n]
          sieve (i,j) = i + j + 2*i*j

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
