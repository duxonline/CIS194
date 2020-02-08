module Week6.Solution where

import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..] 

test1 = print $ take 30 fibs1
-----------------------------------------------------------

fibs2 :: [Integer]
fibs2 = f 0 1
    where f a b = a : f b (a + b)

test2 = print $ take 30 fibs2
-----------------------------------------------------------

data Stream a = Const a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Const x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
    show = show . take 100 . streamToList

fromList :: [a] -> Stream a
fromList a = case cycle a of
    (x:xs) -> Const x (fromList xs)     

test3 = do
    print $ fromList [0..]
    print $ fromList [21..30]
    print $ fromList ['a'..]
-----------------------------------------------------------

streamRepeat :: a -> Stream a
streamRepeat n = Const n (streamRepeat n)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Const x xs) = Const (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Const (f x) (streamFromSeed f (f x))

test4 = do
  print $ streamRepeat 42
  print $ streamMap (*2) (fromList [1..])
  print $ streamFromSeed (*2) 1
-----------------------------------------------------------

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = undefined 

exercise5 = do
  print $ nats