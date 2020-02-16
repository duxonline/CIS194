module Week7.Solution where

import qualified Week7.Lecture as L
import qualified Week7.Sized as S
import qualified Week7.Scrabble as B
import           Data.Char

data JoinList m a = Empty
                    | Single m a
                    | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x <> tag y) x y

tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _ ) = m
tag _ = mempty

someJoinList =
  Append (L.Product 210)
    (Append (L.Product 30)
      (Single (L.Product 5) 'y')
      (Append (L.Product 6)
        (Single (L.Product 2) 'e')
        (Single (L.Product 3) 'a')))
    (Single (L.Product 7) 'h')

(!!?) :: [a] -> Int -> Maybe a
[]      !!? _         =  Nothing
_       !!? n | n < 0 =  Nothing
(x:xs)  !!? 0         =  Just x
(x:xs)  !!? n         =  xs !!? (n-1)

jlToList :: JoinList m a -> [a]
jlToList Empty =        []
jlToList (Single _ a) = [a]
jlToList (Append _ l r) = jlToList l ++ jlToList r

indexJ :: (S.Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ n lst = jlToList lst !!? n

dropJ :: (S.Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl@(Single _ _)
  | n <= 0 = jl
dropJ n jlst@(Append m jl jr)
  | n >= totalSize = Empty
  | n >= leftSize = dropJ (n-leftSize) jr
  | n > 0         = dropJ n jl +++ jr
  | otherwise     = jlst
  where totalSize = S.getSize . S.size $ m
        leftSize = S.getSize . S.size . tag $ jl
dropJ _ _ = Empty

takeJ :: (S.Sized b, Monoid b)
      => Int -> JoinList b a -> JoinList b a
takeJ n jl@(Single _ _)
  | n == 1 = jl
takeJ n jlst@(Append m jl jr)
  | n >= totalSize  = jlst
  | n > leftSize    = jl +++ takeJ (n-leftSize) jr
  | n > 0           = takeJ n jl
  | otherwise       = Empty
  where totalSize = S.getSize . S.size $ m
        leftSize = S.getSize . S.size . tag $ jl
takeJ _ _ = Empty

createList :: JoinList S.Size Char
createList = foldr1 (+++) $ Single (S.Size 1) <$> ['a'..'z']

score :: Char -> B.Score
score c
  | c' `elem` "aeilnorstu" = B.Score 1
  | c' `elem` "dg"         = B.Score 2
  | c' `elem` "bcmp"       = B.Score 3
  | c' `elem` "fhvwy"      = B.Score 4
  | c' `elem` "k"          = B.Score 5
  | c' `elem` "jx"         = B.Score 8
  | c' `elem` "qz"         = B.Score 10
  | otherwise              = B.Score 0
    where c' = toLower c

-- scoreString :: String -> B.Score
-- scoreString = foldr ((+) . score) (B.Score 0)

scoreString :: String -> B.Score
scoreString xs = mconcat (score <$> xs)

scoreLine :: String -> JoinList B.Score String
scoreLine = undefined

test1 = do
  print . L.getProduct . tag $ someJoinList
  print . L.getProduct . tag $ Empty
  print (Empty :: JoinList (L.Product Int) Char)
  print (Empty +++ Empty :: JoinList (L.Product Int) Char)
  print $ Single (L.Product 2) 'a' +++ Single (L.Product 3) 'b'

test2 = do
  print $ takeJ 1 createList
  print $ dropJ 1 createList