newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) intput = p intput

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

string :: String -> Parser String
string [] = return []
string (x:xs) = do  
                char x
                string xs
                return (x:xs)
-- string (x:xs) = 
--   char x    >>= \_ ->
--   string xs >>= \_ -> 
--             return (x:xs)

-- string "abc" => satisfy (=='a') >= \_ ->
--                 string "bc" >= \_ -> return (x:xs)
-- p = satisfy (=='a') => P (\input -> parse (return v) out)
--                     => P ("abcdef" -> parse (return 'a') "bcdef")
-- f = \_ -> string "bc" >= \_ -> return ('a':"bc")
-- p >= f = P (\input    -> parse (f v) out) 
--        =>P (\"abcdef" -> parse (f 'a') "bcdef") 
--        =>P (\"abcdef" -> parse (string "bc" >= \_ -> return ('a':"bc")) "bcdef") 

-- string "bc" => satisfy (=='b') >= \_ ->
--                string "c" >= \_ -> return (x:xs)
-- p = satisfy (=='b') => P (\input -> parse (return v) out) 
--                     => P ("bcdef" -> parse (return 'b') "cdef")
-- f = \_ -> string "c" >= \_ -> return ('b':"c")
-- p >= f = P (\input   -> parse (f v) out) 
--        =>P (\"bcdef" -> parse (f 'b') "cdef") 
--        =>P (\"bcdef" -> parse (string "c" >= \_ -> return ('b':"c")) "cdef") 

-- string "c" => satisfy (=='c') >= \_ ->
--               string "" >= \_ -> return (x:xs)
-- p = satisfy (=='c') => P (\input -> parse (return v) out) 
--                     => P ("cdef" -> parse (return 'c') "def")
-- f = \_ -> string "" >= \_ -> return ('c':"")
-- p >= f = P (\input  -> parse (f v) out) 
--        =>P (\"cdef" -> parse (f 'c') "def") 
--        =>P (\"cdef" -> parse (string "" >= \_ -> return ('c':"")) "def")
--        =>P (\"cdef" -> parse (return [] >= \_ -> return ('c':"")) "def")
--        =>P (\"cdef" -> parse (return []) "def")