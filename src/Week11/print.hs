parse :: Parser a -> String -> [(a, String)]
parse (P p) intput = p intput

instance Monad Parser where
  return v = P (\input -> [(v,input)])
  p >>= f = P (\input -> case parse p input of
                        [] -> []
                        [(v, out)] -> parse (f v) out)

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
                    
testParser = do
  print $ parse (string "abc") "abcdef9"