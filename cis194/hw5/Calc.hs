{-# LANGUAGE FlexibleInstances #-}

module Calc where

import qualified Data.Map as M
import ExprT
import Parser
import qualified StackVM as St

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r

evalStr :: String -> Maybe Integer
evalStr s = return . eval =<< parseExp Lit Add Mul s

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

instance Expr St.Program where
    lit x   = [St.PushI x]
    add x y = x ++ y ++ [St.Add]
    mul x y = x ++ y ++ [St.Mul]

compile :: String -> Maybe St.Program
compile = parseExp lit add mul

class HasVars a where
    var :: String -> a

data ExprT' = Lit' Integer
            | Var' String
            | Add' ExprT' ExprT'
            | Mul' ExprT' ExprT'
    deriving (Show, Eq)

instance Expr ExprT' where
    lit = Lit'
    add = Add'
    mul = Mul'

instance HasVars ExprT' where
    var = Var'

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit x   = \_ -> Just x
    add x y = \m -> case (x m, y m) of (Just x', Just y') -> Just (x' + y')
                                       _                  -> Nothing
    mul x y = \m -> case (x m, y m) of (Just x', Just y') -> Just (x' * y')
                                       _                  -> Nothing

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs expr = expr $ M.fromList vs
