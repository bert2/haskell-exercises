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

-- deriving a type class

data Fruit = Apple | Orange deriving Show
whichFruit f =
    case f of
        "apple" | False -> Apple
        "orange" -> Orange

-- implementing a type class

data Foo = F Int | G Char

instance Eq Foo where
    (F i1) == (F i2) = i1 == i2
    (G c1) == (G c2) = c1 == c2
    _      == _      = False

-- our own type class

class Listable a where
    toList :: a -> [Int]
instance Listable Int where
    toList x = [x]
instance Listable Bool where
    toList True  = [1]
    toList False = [0]
instance (Listable a, Listable b) => Listable (a,b) where
    toList (x,y) = toList x ++ toList y
instance Listable a => Listable [a] where
    toList = concatMap toList

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show
instance Listable (Tree Int) where
    toList Empty        = []
    toList (Node x l r) = toList l ++ [x] ++ toList r
