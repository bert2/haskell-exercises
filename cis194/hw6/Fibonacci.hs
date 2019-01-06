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
fibs3 = 0 : 1 : zipWith (+) fibs3 (tail fibs3)
fibs4 = map fst $ iterate (\(x,y) -> (y,x+y)) (0,1)
fibs5 = 0 : 1 : next fibs5
    where next (x:ts@(y:_)) = (x+y) : next ts
