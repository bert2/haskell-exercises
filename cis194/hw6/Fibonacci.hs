{-# LANGUAGE FlexibleInstances #-}
import Data.List
import Data.Ratio
import Text.Printf

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : fibs' 0 1
    where fibs' x y = let z = x + y
                      in z : fibs' y z

-- https://wiki.haskell.org/The_Fibonacci_sequence
fibs2' = 0 : 1 : zipWith (+) fibs2' (tail fibs2')
fibs2'' = map fst $ iterate (\(x,y) -> (y,x+y)) (0,1)
fibs2''' = 0 : 1 : next fibs2'''
    where next (x:ts@(y:_)) = (x+y) : next ts

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
    show xs = let firstFew = map show $ take 40 $ streamToList xs
              in "[" ++ intercalate "," firstFew ++ ",...]"

infixr 5 !:
(!:) :: a -> Stream a -> Stream a
(!:) = Cons

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

streamRepeat :: a -> Stream a
streamRepeat x = x !: streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = f x !: streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = x !: streamFromSeed f (f x)

streamMerge :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
streamMerge f (Cons x xs) (Cons y ys) = f x y !: streamMerge f xs ys

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons x xs) ys = x !: streamInterleave ys xs

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler' :: Stream Integer
ruler' = streamMap f $ streamFromSeed (+1) 1
    where f x = let pows = streamToList $ streamFromSeed (\(e,p) -> (e+1,p*2)) (0,1)
                in fst $ last $ takeWhile (\(_,p) -> p<=x && x `mod` p == 0) pows

ruler :: Stream Integer
ruler = let gen n = streamInterleave (streamRepeat n) (gen (n+1))
        in gen 0

x :: Stream Integer
x = 0 !: 1 !: streamRepeat 0

instance Num (Stream Integer) where
    fromInteger n = n !: streamRepeat 0
    negate = streamMap negate
    (+) = streamMerge (+)
    (*) (Cons a as') bs@(Cons b bs') = a*b !: streamMap (*a) bs' + as'*bs
    abs = streamMap abs
    signum = streamMap signum

instance Fractional (Stream Integer) where
    fromRational a = undefined
    (/) (Cons a as') (Cons b bs') = q
        where q = a `div` b !: streamMap (`div` b) (as' - q*bs')

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x*x)

data Matrix = Matrix Integer Integer Integer Integer

instance Show (Matrix) where
    show (Matrix m11 m12 m21 m22) =
        let fmt1 = fmt $ maxWidth m11 m21
            fmt2 = fmt $ maxWidth m12 m22
        in "|" ++ fmt1 m11 ++ " " ++ fmt2 m12 ++ "|\n" ++
           "|" ++ fmt1 m21 ++ " " ++ fmt2 m22 ++ "|"
        where fmt = printf "%*d"
              maxWidth a b = maximum $ map (length . show) [a,b]

instance Num (Matrix) where
    fromInteger n = (Matrix n 0 0 n)
    negate = (*(-1))
    (+) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22) =
        (Matrix (a11+b11) (a12+b12) (a21+b21) (a22+b22))
    (*) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22) =
        (Matrix (a11*b11 + a12*b21) (a11*b12 + a12*b22) (a21*b11 + a22*b21) (a21*b12 + a22*b22))
    abs = id
    signum (Matrix m11 m12 m21 m22) =
        (Matrix (signum m11) (signum m12) (signum m21) (signum m22))

fib4 :: Integer -> Integer
fib4 n = let m = Matrix 1 1 1 0
             (Matrix _ m12 _ _) = m^n
         in m12
