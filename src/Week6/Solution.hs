module Week6.Solution where

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
    show = show . take 20 . streamToList

fromList :: [a] -> Stream a
fromList a = case cycle a of
    (x:xs) -> Const x (fromList xs)     

test3 = do
    print $ fromList [0..]
    print $ fromList [21..30]
    print $ fromList ['a'..]
-----------------------------------------------------------
