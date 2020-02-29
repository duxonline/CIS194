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

-- f                    :: a -> b
-- first                :: (a -> b) -> (a, String) -> (b, String)
-- first f              :: (a, String) -> (b, String)
-- fmap                 :: ((a, String) -> (b, String)) -> Maybe (a, String) -> Maybe (b, String)
-- fmap (first f)       :: Maybe (a, String) -> Maybe (b, String)
-- p                    :: String -> Maybe (a, String)
-- fmap (first f) . p   :: String -> Maybe (b, String)
instance Functor Parser where
    fmap f (Parser p) = Parser g
        where g = fmap (first f) . p

test1 = do
  print $ first toLower ('A', "BC")
  print $ runParser (satisfy isUpper) "ABC"
  print $ runParser (satisfy isUpper) "aBC"
  print $ runParser (toLower <$> satisfy isUpper) "ABC"

-- <*>      :: f (a -> b) -> f a -> f b
-- <*>      :: Parser (a -> b) -> Parser a -> Parser b
-- xs       :: String
-- f        :: String -> Maybe (a -> b, String)
-- f xs     :: Maybe (a -> b, String)
-- k        :: (a -> b, String) -> Maybe (b, String)
-- p        :: a -> b
-- first    :: (a -> b) -> (a, String) -> (b, String)
-- first p  :: (a, String) -> (b, String)
-- ys       :: String
-- g        :: String -> Maybe (a, String)
-- g ys     :: Maybe (a, String)
-- <$>      :: ((a, String) -> (b, String)) -> Maybe (a, String) -> Maybe (b, String)
-- >>=      :: Maybe (a -> b, String) -> ((a -> b, String) -> Maybe (b, String)) -> Maybe (b, String)
-- >>=      :: m a -> (a -> m b) -> m b
instance Applicative Parser where
    pure a = Parser $ \xs -> Just (a, xs)
    Parser f <*> Parser g = Parser h
        where
        h s = f s >>= k
        k (p, ys) = first p <$> g ys

type Name = String
data Employee = Emp {name :: Name, phone :: String}
    deriving(Show)

-- <$> :: (Name -> String -> Employee) -> Parser Name -> Parser (String -> Employee)
-- <*> :: Parser (String -> Employee) -> Parser String -> Parser Employee
parseEmployee :: Parser Employee
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