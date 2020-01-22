module Week1.Solution where

type Peg = String

toDigits :: Int -> [Int]
toDigits n
    | n > 0 = toDigits (n `div` 10) ++ [n `mod` 10]
    | otherwise = []

toDigitsRev :: Int -> [Int]
toDigitsRev n
    | n > 0 = n `mod` 10 : toDigitsRev(n `div` 10)
    | otherwise = []

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther l = reverse $ zipWith (*) (reverse l) $ cycle [1,2]

sumDigits :: [Int] -> [Int]
sumDigits = concatMap toDigits

validate :: Int -> Bool
validate n = 
    case (sum . sumDigits . doubleEveryOther . toDigits) n `mod` 10 of
        0 -> True 
        _ -> False 

reverse2 :: [Int] -> [Int]
reverse2 [] = []
reverse2 (x:zs) = reverse2 zs ++ [x]

hanoi :: Int -> Peg -> Peg -> Peg -> [(Peg, Peg)]
hanoi 0 _ _ _ = []
hanoi 1 src target temp = [(src, target)]
hanoi n src target temp = hanoi (n-1) src temp target ++ [(src,target)] ++ hanoi (n-1) temp target src