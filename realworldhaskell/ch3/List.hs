data List a = Cons a (List a)
            | Nil
    deriving (Show, Eq)

fromList' :: [a] -> List a
fromList' []     = Nil
fromList' (x:xs) = Cons x (fromList' xs)

toList' :: List a -> [a]
toList' Nil         = []
toList' (Cons x xs) = x : toList' xs

foldr' :: (a -> b -> b) -> b -> List a -> b
foldr' _ z Nil         = z
foldr' f z (Cons x xs) = let z' = foldr' f z xs
                         in  x `f` z'

foldl' :: (b -> a -> b) -> b -> List a -> b
foldl' _ z Nil         = z
foldl' f z (Cons x xs) = let z' = z `f` x
                         in  seq z' $ foldl' f z' xs

filter' :: (a -> Bool) -> List a -> List a
filter' p = foldr' consIf Nil
    where consIf x = if p x then Cons x else id

(+++) :: List a -> List a -> List a
xs +++ ys = foldr' Cons ys xs

concat' :: List (List a) -> List a
concat' = foldr' (+++) Nil

intersperse' :: a -> List a -> List a
intersperse' _   Nil          = Nil
intersperse' _   (Cons x Nil) = Cons x Nil
intersperse' sep (Cons x xs)  = Cons x $ Cons sep $ intersperse' sep xs

intercalate' :: List a -> List (List a) -> List a
intercalate' xs = concat' . intersperse' xs

intercalerse' :: a -> List (List a) -> List a
intercalerse' sep = intercalate' (Cons sep Nil)

null' :: List a -> Bool
null' Nil = True
null' _   = False

length' :: List a -> Int
length' = foldr' (\_ z -> z + 1) 0

head' :: List a -> a
head' = foldr' (\x _ -> x) (error "empty list")

last' :: List a -> a
last' = foldl' (\_ x -> x) (error "empty list")

init' :: List a -> List a
init' Nil          = error "empty list"
init' (Cons _ Nil) = Nil
init' (Cons x xs)  = Cons x (init' xs)

tail' :: List a -> List a
tail' Nil          = error "empty list"
tail' (Cons _ xs)  = xs

app' :: a -> List a -> List a
app' x = foldr' Cons (Cons x Nil)

reverse' :: List a -> List a
reverse' = foldl' (flip Cons) Nil

sum' :: Num a => List a -> a
sum' = foldr' (+) 0

product' :: Num a => List a -> a
product' = foldr' (*) 1

mean' :: Fractional a => List a -> a
mean' xs = sum' xs / fromIntegral (length' xs)

toPalindrome' :: List a -> List a
toPalindrome' xs = xs +++ reverse' xs

palindrome' :: Eq a => List a -> Bool
palindrome' xs = xs == (reverse' xs)

sortBy' :: (a -> a -> Ordering) -> List a -> List a
sortBy' _ Nil         = Nil
sortBy' f (Cons x xs) = smallers +++ Cons x biggers
    where smallers = sortBy' f $ filter' (\x' -> f x' x <= EQ) xs
          biggers  = sortBy' f $ filter' (\x' -> f x' x == GT) xs

sortOn' :: Ord b => (a -> b) -> List a -> List a
sortOn' f = sortBy' (\x x' -> compare (f x) (f x'))

sort' :: Ord a => List a -> List a
sort' = sortBy' compare
