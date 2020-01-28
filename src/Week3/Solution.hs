module Week3.Solution where

skips :: [a] -> [[a]]
skips lst = [each i lst | i <- [1..length lst -1]]

each :: Int -> [a] -> [a]
each n lst = [ lst !! i | i <- [n-1, n-1+n..length lst -1]]

localMaxima :: [Int] -> [Int]
localMaxima (x:y:z:zx)
    | x < y && y > z = y : localMaxima (y:z:zx)
    | otherwise = localMaxima (y:z:zx)
localMaxima _ = []

histogram :: [Int] -> String
histogram lst =
    unlines (map (drawLine fs) [max+1, max..1]) ++ "==========\n0123456789\n"
    where
        fs = frequency lst
        max = maximum fs

drawLine :: [Int] -> Int -> String
drawLine lst n = [if x >= n then '*' else ' ' | x <- lst]

frequency :: [Int] -> [Int]
frequency lst = [length (filter (==x) lst) | x <- [0..9]]