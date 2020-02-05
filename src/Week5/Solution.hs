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

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit x
        | x <= 0 = False 
        | x > 0 = True
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = MinMax (x + y)
    mul (MinMax x) (MinMax y) = MinMax (x * y)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit = Mod7 . (`mod` 7)
    add (Mod7 x) (Mod7 y) = Mod7 ((x+y) `mod` 7)
    mul (Mod7 x) (Mod7 y) = Mod7 ((x*y) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"