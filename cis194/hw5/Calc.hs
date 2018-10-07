module Calc where

import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
                Just expr -> Just (eval expr)
                _         -> Nothing

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (>0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    lit                       = MinMax
    add (MinMax x) (MinMax y) = lit (max x y)
    mul (MinMax x) (MinMax y) = lit (min x y)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit                   = Mod7 . (`mod` 7)
    add (Mod7 x) (Mod7 y) = lit (x + y)
    mul (Mod7 x) (Mod7 y) = lit (x * y)
