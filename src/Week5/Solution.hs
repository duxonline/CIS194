module Week5.Solution where

import Week5.ExprT

eval :: ExprT -> Integer
eval (Lit l) = l
eval (Add exp1 exp2) = eval exp1 + eval exp2
eval (Mul exp1 exp2) = eval exp1 * eval exp2