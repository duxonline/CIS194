module Week3.Solution where

skips :: [a] -> [[a]]    
skips list = [each i list | i <- [1..length list -1]]

each :: Int -> [a] -> [a]
each n list = [ list !! i | i <- [n-1, n-1+n..length list -1]]

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zx)
    | x < y && y > z = y : localMaxima (y:z:zx)
    | otherwise = localMaxima (y:z:zx)
localMaxima _ = []


frequency :: [Int] -> [Int]
frequency lst = [ length (filter (==x) lst) | x <- [0..9]]