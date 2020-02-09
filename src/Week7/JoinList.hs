module Week7.JoinList where

import qualified Week7.Lecture as L

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

test1 = do
  print . L.getProduct . tag $ someJoinList
  print . L.getProduct . tag $ Empty
  print (Empty :: JoinList (L.Product Int) Char)
  print (Empty +++ Empty :: JoinList (L.Product Int) Char)
  print $ Single (L.Product 2) 'a' +++ Single (L.Product 3) 'b'