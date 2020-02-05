module Week5.Solution where

import Week5.ExprT
import Week5.Parser

eval :: ExprT -> Integer
eval (Lit l) = l
eval (Add exp1 exp2) = eval exp1 + eval exp2
eval (Mul exp1 exp2) = eval exp1 * eval exp2

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
            Nothing -> Nothing 
            Just p -> Just $ eval p

class Expr a where 
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

reify :: ExprT -> ExprT
reify = id

-- :t mul (add (lit 2) (lit 3)) (lit 4)
-- :t reify $ mul (add (lit 2) (lit 3)) (lit 4)