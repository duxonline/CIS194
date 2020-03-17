-- Sample code from Programming in Haskell

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
  -- return :: a -> Parser a
  return v = P (\input -> [(v,input)])
  
  -- >>= :: Parser a -> (a -> Parser b) -> Parser b
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
string (x:xs) = 
  char x    >>= \_ ->
  string xs >>= \_ -> 
            return (x:xs)
-- string (x:xs) = do  char x
--                     string xs
--                     return (x:xs)


-- string "abc" => satisfy (=='a') >= \_ ->
--                 string "bc" >= \_ -> return (x:xs)

-- p = satisfy (=='a') => P (\input -> parse (return v) out) 
--                     => P("abcdef" -> parse (return 'a') "bcdef")
-- f = \_ -> string "bc" >= \_ -> return (x:xs)
-- p >= f = P (\input -> parse (f v) out) 
--        =>P (\"abcdef" -> parse (f 'a') 'bcdef') 


-- string "bc" => satisfy (=='b') >= \_ ->
--                 string "c" >= \_ -> return (x:xs)

-- string "c" => satisfy (=='c') >= \_ ->
--                 string "" >= \_ -> return (x:xs)


main = do
  print $ parse three "abcedf"
  print $ parse (string "abc") "abcdef"