{-# LANGUAGE FlexibleInstances #-}
module Week5.Solution where

import Week5.ExprT
import Week5.Parser
import qualified Week5.StackVM as S
import qualified Data.Map as M
import Data.Maybe

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
    lit x = x > 0
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

instance Expr S.Program where
    lit i = [S.PushI i]
    add a b = a ++ b ++ [S.Add]
    mul a b = a ++ b ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = parseExp lit add mul

-- compile "(2+3)*4"

evalStack :: String -> Either String S.StackVal
evalStack = S.stackVM . fromMaybe [] . compile

-- evalStack "(2+3)*4"

class HasVars a where
    var :: String -> a

data VarExprT = VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              | Var String
    deriving (Eq, Show)

instance Expr VarExprT where
    lit = VLit
    add = VAdd
    mul = VMul

instance HasVars VarExprT where
    var = Var


type MapExpr = M.Map String Integer -> Maybe Integer

instance HasVars MapExpr where
    var = M.lookup

instance Expr MapExpr where
    lit a = (\_ -> Just a)
    add a b = (\vs -> (+) <$> a vs <*> b vs)
    mul a b = (\vs -> (*) <$> a vs <*> b vs)

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs mexp = mexp $ M.fromList vs

test = do
  print $ withVars [("x", 6)] $ add (lit 3) (var "x")
  print $ withVars [("x", 6)] $ add (lit 3) (var "y")
  print $ withVars [("x", 6), ("y", 3)]
    $ mul (var "x") (add (var "y") (var "x"))