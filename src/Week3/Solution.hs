module Week3.Solution where

skips :: [a] -> [[a]]    
skips list = [each i list | i <- [1..length list -1]]

each :: Int -> [a] -> [a]
each n list = [ list !! i | i <- [n-1, n-1+n..length list -1]]