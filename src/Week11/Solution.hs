module Week11.Solution where

import  Week11.AParser
import  Data.Char
import Control.Applicative

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure [] 

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

test1 = do
    print $ runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH"
    print $ runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH"
    print $ runParser (zeroOrMore (satisfy isUpper)) "abcdEfgH"
    print $ runParser (oneOrMore (satisfy isUpper)) "abcdEfgH"