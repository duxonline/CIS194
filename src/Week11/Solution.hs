module Week11.Solution where

import  Week11.Aparser
import  Data.Char

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore = undefined 


oneOrMore :: Parser a -> Parser [a]
oneOrMore = undefined

test1 = do
    print $ runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH"
    print $ runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH"
    print $ runParser (zeroOrMore (satisfy isUpper)) "abcdEfgH"
    print $ runParser (oneOrMore (satisfy isUpper)) "abcdEfgH"