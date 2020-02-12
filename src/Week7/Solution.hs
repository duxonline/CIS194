module Week7.Solution where

import qualified Week7.Lecture as L
import qualified Week7.Sized as S

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
dropJ n jl@(Append m jl1 jl2)
  | n >= size0 = Empty
  | n < size1  = dropJ n jl1 +++ jl2
  | n > 0      = dropJ (n - size1) jl2
  | otherwise  = jl
    where
      size0 = S.getSize . S.size $ m
      size1 = S.getSize . S.size . tag $ jl1
dropJ _ _ = Empty

takeJ :: (S.Sized b, Monoid b)
      => Int -> JoinList b a -> JoinList b a
takeJ = undefined

az :: JoinList S.Size Char
az = foldr1 (+++) $ Single (S.Size 1) <$> ['a'..'z']

test1 = do
  print . L.getProduct . tag $ someJoinList
  print . L.getProduct . tag $ Empty
  print (Empty :: JoinList (L.Product Int) Char)
  print (Empty +++ Empty :: JoinList (L.Product Int) Char)
  print $ Single (L.Product 2) 'a' +++ Single (L.Product 3) 'b'