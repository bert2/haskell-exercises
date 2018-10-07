module Calc where

import ExprT

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r
