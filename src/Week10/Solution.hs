module Week10.Solution where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.List.Split

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
    where
        f [] = Nothing
        f (x:xs)
            | p x = Just (x, xs)
            | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
    where 
        f xs
            | null ns = Nothing 
            | otherwise  = Just (read ns, rest)
            where (ns, rest) = span isDigit xs

first :: (a -> b) -> (a,c) -> (b,c)
first f (a ,b) = (f a, b)

instance Functor Parser where
    fmap f (Parser p) = Parser $ fmap (first f) . p

test1 = do
  print $ first toLower ('A', "BC")
  print $ runParser (satisfy isUpper) "ABC"
  print $ runParser (toLower <$> satisfy isUpper) "ABC"

instance Applicative Parser where
    pure a = Parser $ \xs -> Just (a, xs)
    Parser f <*> Parser g = Parser $ \xs -> f xs >>= h
        where
        h (p, ys) = first p <$> g ys

type Name = String
data Employee = Emp {name :: Name, phone :: String}
    deriving(Show)

parseEmployee = Emp <$> parseName <*> parsePhone
    where
        parsePhone = show <$> posInt
        parseName = Parser f
            where
                f [] = Nothing
                f xs = case splitOn ", " xs of
                    [a, b] -> Just (a, b)
                    _      -> Nothing

test2 = do
  print $ runParser parseEmployee "John Moe, 2564213"
  print $ runParser parseEmployee "Melissa13"
  print $ runParser (Emp <$> pure "Jessica" <*> show <$> posInt) "8675309"