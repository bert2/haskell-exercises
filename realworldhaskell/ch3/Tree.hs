module Tree where

data Tree a = Node a (Tree a) (Tree a)
            | Leaf
    deriving Show

tsum :: Num a => Tree a -> a
tsum Leaf           = 0
tsum (Node x ls rs) = tsum ls + x + tsum rs

theight :: Tree a -> Int
theight Leaf           = 0
theight (Node _ ls rs) = 1 + max (theight ls) (theight rs)
