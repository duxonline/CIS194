module Week4.Solution where

import Data.List

-- fun1 :: [Integer] -> Integer
-- fun1 [] = 1
-- fun1 (x:xs)
--     | even x = (x -2) * fun1 xs
--     | otherwise = fun1 xs

fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

-- fun2 :: Integer -> Integer
-- fun2 1 = 0
-- fun2 n 
--     | even n = n + fun2 (n `div` 2)
--     | otherwise = fun2 (3 * n + 1)

fun2 :: Integer -> Integer
fun2 = sum
      .filter even
      .takeWhile (/=1)
      .iterate (\n -> if even n then n `div` 2 else 3 * n +1)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree lst = Node height
               (foldTree $ take half lst)
               (lst !! half)
               (foldTree $ drop (half+1) lst)
            where
                len = length lst
                half = len `div` 2
                height = floor (logBase 2 (fromIntegral len))

-- xor :: [Bool] -> Bool
-- xor = odd . length . filter (==True)

xor :: [Bool] -> Bool
xor = foldr (\x y -> (x || y) && not(x && y)) False

myFoldL :: (a -> b -> a) -> a -> [b] -> a
myFoldL f z lst = foldr (flip f) z (reverse lst)

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = (+1) . (*2) <$> [1..n] \\ nonPrimes
  where
    nonPrimes = takeWhile (<n) $ map (uncurry ij2ij) $ cartProd [1..] [1..]
      where
        ij2ij i j = i + j + 2 * i * j

sieveSundaram2 :: Integer -> [Integer]
sieveSundaram2 n = map ((+1) . (*2)) $ [1..n] \\ sieve
    where 
        sieve = map (\(i,j) -> i + j + 2 * i * j) 
                . filter (\(i,j) -> i + j + 2 * i * j <= n) 
                $ cartProd [1..n] [1..n]

sieveSundaram3 :: Integer -> [Integer]
sieveSundaram3 n = map ((+1) . (*2)) $ [1..n] \\ sieve
  where sieve = filter (<=n)
                . map (\(i, j) -> i + j + 2*i*j)
                $ cartProd [1..n] [1..n]
        
cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]