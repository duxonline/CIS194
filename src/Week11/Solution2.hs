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
  empty = P (const [])
  -- empty = P (\input -> [])
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

-- one = fmap g item => P(\input -> [(g v, out)])
-- one = fmap g item => P(\"abcde" -> [(g 'a', "bcde")])
one :: Parser Char
one  = g <$> item
       where g x = x

-- g :: Char -> Char -> (Char, Char)
-- g :: Char -> (Char -> (Char, Char))
-- g' = g 'a' = Char -> ('a', Char)
-- pg = (g <$> item) => P(\input -> [(g v, out)])
--                   => P(\"abcde" -> [(g 'a', "bcde")])
--                   => P(\"abcde" -> [(g', "bcde")])
-- pg <*> item => pg <*> px => P(\input -> parse (fmap g' item) out)
--                          => P(\"abcde" -> P(\"bcde" -> [(g 'b', "cde")]))
--                          => P(\"abcde" -> P(\"bcde" -> [('a','b'), "cde")]))
two :: Parser (Char, Char)
two  = g <$> item <*> item
       where g x y = (x, y)

-- pg = pure g = P (\input -> [(g, input)]) = P p 
--        => p = (\input -> [(g, input)])
-- px = item
-- pg <*> px = P p <*> item = P (\input -> parse (fmap p item) out) =>
-- 
-- P p <*> item <*> item <*> item
three' :: Parser (Char, Char, Char)
three' =  pure g <*> item <*> item <*> item
          where g x y z = (x, y, z)
-- three2 =  g <$> item <*> item <*> item
--           where g x y z = (x, z)

-- p = item
-- f = \x -> item >>= \_ -> item >>= \z -> return (x,z)
-- p >>= f = P(\input -> [(x,xs)] >> f
-- = P(\input -> parse (f 'a') out) 
-- = P(\"abcdef" -> parse (item >>= \_ -> item >>= \z -> return ('a', z)) out)
-- = P(\"abcdef" -> parse (item >>= \_ -> item >>= \z -> return ('a', z)) "bcdef")

-- p' = item = P(\input -> [(x,xs)]) = P(\"bcdef" -> [('b',"cdef")])
-- f' = \_ -> item >>= \z -> return ('a',z)
-- p' >= f' = P(\input -> parse (f v) out) 
--          = P(\"bcdef" -> parse(f 'b') "cdef")
--          = P(\"bcdef" -> parse(item >>= \z -> return ('a',z)) "cdef")

-- p'' = item = P(\input -> [(x,xs)]) = P(\"cdef" -> [('c',"def")])
-- f'' = \z -> return ('a',z)
-- p'' >= f'' = P(\input -> parse (f v) out)
--            = P(\"cdef" -> parse(f 'c') "def")
--            = P(\"cdef" -> parse(return ('a','c')) "def")
--            = P(\"cdef" -> parse(P(\input -> [(('a','c'), input)]) "def")
--            = [(('a','c'), "def")]

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
-- string (x:xs) = do
--                 char x
--                 string xs
--                 return (x:xs)
string (x:xs) = 
  char x    >>= \_ ->
  string xs >>= \_ -> 
            return (x:xs)

-- string "abc" => satisfy (=='a') >= \_ -> string "bc" >= \_ -> return (x:xs)
-- p = satisfy (=='a') => P (\input -> parse (return v) out)
--                     => P ("abcdef" -> parse (return 'a') "bcdef")
-- f = \_ -> string "bc" >= \_ -> return ('a':"bc")
-- p >= f = P (\input    -> parse (f v) out) 
--        =>P (\"abcdef" -> parse (f 'a') "bcdef") 
--        =>P (\"abcdef" -> parse (string "bc" >= \_ -> return ('a':"bc")) "bcdef") 

-- string "bc" => satisfy (=='b') >= \_ -> string "c" >= \_ -> return (x:xs)
-- p = satisfy (=='b') => P (\input -> parse (return v) out) 
--                     => P ("bcdef" -> parse (return 'b') "cdef")
-- f = \_ -> string "c" >= \_ -> return ('b':"c")
-- p >= f = P (\input   -> parse (f v) out) 
--        =>P (\"bcdef" -> parse (f 'b') "cdef") 
--        =>P (\"bcdef" -> parse (string "c" >= \_ -> return ('b':"c")) "cdef") 

-- string "c" => satisfy (=='c') >= \_ -> string "" >= \_ -> return (x:xs)
-- p = satisfy (=='c') => P (\input -> parse (return v) out) 
--                     => P ("cdef" -> parse (return 'c') "def")
-- f = \_ -> string "" >= \_ -> return ('c':"")
-- p >= f = P (\input  -> parse (f v) out) 
--        =>P (\"cdef" -> parse (f 'c') "def") 
--        =>P (\"cdef" -> parse (string "" >= \_ -> return ('c':"")) "def")
--        =>P (\"cdef" -> parse (return [] >= \_ -> return ('c':"")) "def")
--        =>P (\"cdef" -> parse (return []) "def")

test = do
  print $ parse three "abcedf"
  print $ parse three' "abcedf"
  print $ parse (string "abc") "abcdef"
  print $ parse (return []::Parser String) "def"