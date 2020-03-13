module Week11.Solution2 where
    
import Control.Applicative

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) intput = p intput

instance Functor Parser where
  fmap g p = P (\input -> case parse p input of
                        [] -> []
                        [(v,out)] -> [(g v, out)])

instance Applicative Parser where
  pure v = P (\input -> [(v,input)])
  pg <*> px = P (\input -> case parse pg input of
                        [] -> []
                        [(g, out)] -> parse (fmap g px) out)

instance Monad Parser where
  return v = P (\input -> [(v,input)])
  p >>= f = P (\input -> case parse p input of
                        [] -> []
                        [(v, out)] -> parse (f v) out)

instance Alternative Parser where
  empty = P (\input -> [])
  p <|> q = P (\input -> case parse p input of
                        [] -> parse q input
                        [(v,out)] -> [(v,out)])                         

item :: Parser Char
item = P (\input -> case input of
                    [] -> []
                    (x:xs) -> [(x, xs)])

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do 
              x <- item
              if p x then return x else empty

char :: Char -> Parser Char
char x = satisfy (==x)

three :: Parser (Char, Char)
-- three = do 
--         x <- item
--         item
--         z <- item
--         return (x, z)
three = 
  item >>= \x ->
  item >>= \_ ->
  item >>= \z ->
          return (x,z)

string :: String -> Parser String
string [] = return []
string (x:xs) = do  char x
                    string xs
                    return (x:xs)

main = do
  print $ parse three "abcedfg"
  print $ parse (string "abc") "abcdef9"