module Week4.Solution where

-- fun1 :: [Integer] -> Integer
-- fun1 [] = 1
-- fun1 (x:xs)
--     | even x = (x -2) * fun1 xs
--     | otherwise = fun1 xs

fun1 :: [Integer] -> Integer
fun1 = product . filter even