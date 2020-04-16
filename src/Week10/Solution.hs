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
char c = satisfy (==c)

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
-- f s      :: Maybe (a -> b, String)
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

-- (,) :: a -> b -> (a, b)
-- char 'a' :: Parser Char
-- (,) <$> char 'a' :: Parser (b -> (Char, b))
-- (,) <$> char 'a' <*> char 'b' :: Parser (Char, Char)
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

-- const                    :: a -> b -> a
-- const ()                 :: b -> ()
-- abParser                 :: Parser (Char, Char)
-- <$>                      :: Functor f => (a -> b) -> f a -> f b
-- <$>                      :: ((Char, Char) -> ()) -> Parser (Char, Char) -> Parser ()
-- const () <$> abParser    :: Parser ()
abParser_ :: Parser ()
-- abParser_ = const () <$> abParser
abParser_ = () <$ abParser

merge :: Integer -> Char -> Integer -> [Integer]
merge a p b =
    case p of
        ' ' -> [a,0,b]
        _   -> [a,b]

-- (\a _ b -> [a,b]) :: a -> p -> a -> [a]
-- (\a _ b -> [a,b]) <$> posInt :: Parser (p -> Integer -> [Integer])
-- (\a _ b -> [a,b]) <$> posInt <*> char ' ' :: Parser (Integer -> [Integer])
-- (\a _ b -> [a,b]) <$> posInt <*> char ' ' <*> posInt :: Parser [Integer]
intPair :: Parser [Integer]
intPair = merge <$> posInt <*> char ' ' <*> posInt

test3 = do
--   print $ runParser posInt "23"
--   print $ runParser (char 'a') "a23"
  print $ runParser intPair "23 34"

-- runParser intPair "23 34"
-- runParser (\a _ b -> [a,b]) <$> posInt <*> char ' ' <*> posInt "23 34"
-- runParser (fmap f posInt) <*> char ' ' <*> posInt "23 34"
-- f :: a -> (p -> b -> [a, b])
-- f = (\a _ b -> [a,b])
-- runParser (fmap f (Parser p)) <*> char ' ' <*> posInt "23 34"
-- p :: String -> Maybe (Integer, String)
-- p xs
--     | null ns = Nothing
--     | otherwise  = Just (read ns, rest)
--     where (ns, rest) = span isDigit xs

-- runParser Parser f' <*> char ' ' <*> posInt "23 34"
-- f' = fmap (first f) . p 
-- runParser Parser f' <*> Parser g' <*> posInt "23 34"
-- g' :: String -> Maybe (Char, String)
-- g' [] = Nothing
-- g' (x:xs)
--     | (==' ') x = Just (x, xs)
--     | otherwise = Nothing

instance Alternative Parser where
    empty = Parser (const Nothing)
    (<|>) f g  = Parser h
        where
            h xs = runParser f xs <|> runParser g xs

test4 = do
    print $ runParser (char 'a' <|> char 'b') "ab"
    print $ runParser (char 'b' <|> char 'a') "ba"
    print $ runParser (char 'b' <|> char 'z') "ab"

intOrUppercase :: Parser ()
intOrUppercase = (const () <$> posInt) <|> (const () <$> satisfy isUpper)
-- intOrUppercase = (() <$ posInt) <|> (() <$ satisfy isUpper)

test5 = do
    print $ runParser intOrUppercase "123abc"
    print $ runParser intOrUppercase "AZyz"
    print $ runParser intOrUppercase "abcd"