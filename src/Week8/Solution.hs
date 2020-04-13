module Week8.Solution where

import Week8.Employee
import Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons emp (GuestList emps funs) = GuestList (emp:emps) (empFun emp + funs)

instance Semigroup GuestList where
    (<>) (GuestList e1 f1) (GuestList e2 f2) = GuestList (e1 ++ e2) (f1 +f2)

instance Monoid GuestList where
    mempty = GuestList [] 0
    mappend (GuestList e1 f1) (GuestList e2 f2) = GuestList (e1 ++ e2) (f1 +f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

test1 = do
  print $ glCons e mempty <> gl
  print $ moreFun gl (glCons e mempty)
    where
      e = Emp "Sharon" 60
      gl = GuestList [Emp "Frank" 47, Emp "Mary" 15 ] 62

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x xs) = f x (fmap (treeFold f) xs)

test2 = do
  let tree = Node 1 [Node 2 [Node 3 [], Node 4 []], Node 5 []]
  print $ foldTree (\x xs -> sum (x:xs)) tree
-- f = (\x xs -> sum (x:xs))
-- x = 1
-- xs = [Node 2 [Node 3 [], Node 4 []], Node 5 []]
-- f 1 (fmap (treeFold f) [Node 2 [Node 3 [], Node 4 []], Node 5 []])
-- f 1 [treeFold f Node 2 [Node 3 [], Node 4 []], treeFold f Node 5 []]
-- f 1 [f 2 (fmap (treeFold f) [Node 3 [], Node 4 []]), treeFold f Node 5 []]
-- f 1 [f 2 [treeFold f Node 3 [], treeFold f Node 4 []], treeFold f Node 5 []]
-- f 1 [f 2 [treeFold f Node 3 [], treeFold f Node 4 []], treeFold f Node 5 []]
-- f 1 [f 2 [f 3 [], f 4 []], f 5 []]
-- f 1 [f 2 [3, 4], 5]
-- f 1 [9, 5]
-- 15

  print $ foldTree (\x xs -> maximum (x:xs)) tree
-- f = (\x xs -> maximum (x:xs))
-- f 1 [f 2 [f 3 [], f 4 []], f 5 []]
-- f 1 [f 2 [3, 4], f 5 []]
-- f 1 [4, 5]
-- 5

  print $ foldTree (\_ xs -> if null xs then 1 else sum xs) tree
-- f = (\_ xs -> if null xs then 1 else sum xs)
-- f 1 [f 2 [f 3 [], f 4 []], f 5 []]
-- f 1 [f 2 [1, 1],1]
-- f 1 [sum [1,1],1]
-- sum [sum [1,1],1]
-- sum [2,1]
-- 3

  print $ foldTree (\_ xs -> if null xs then 0 else 1 + maximum xs) tree
-- f = (\_ xs -> if null xs then 0 else 1 + maximum xs)
-- f 1 [f 2 [f 3 [], f 4 []], f 5 []]
-- f 1 [f 2 [0, 0], 0]
-- f 1 [1 + max[0, 0], 0]
-- f 1 [1, 0]
-- 1 + max[1, 0]
-- 2

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss results = (withBoss, withoutBoss) where
    withoutBoss = mconcat (map (uncurry moreFun) results)
    withBoss = glCons boss (mconcat (map snd results))

nextLevel2 :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel2 boss results = (withBoss, withoutBoss)
  where
    withoutBoss = foldMap (uncurry moreFun) results
    withBoss = glCons boss $ foldMap snd results

test3 =
  print $ nextLevel boss guestLists
    where
      boss = Emp "Joe" 5
      guestLists = [(GuestList [Emp "Stan" 9] 9, GuestList [Emp "Bob" 3] 3)]

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

test4 = print $ maxFun testCompany

formatGL :: GuestList -> String
formatGL (GuestList lst fun) = "Total fun: " ++ show fun ++ "\n" ++ unlines (empName <$> lst)

test5 = readFile "src/Week8/company.txt" >>= computeGuestList >>= putStr
  where
    computeGuestList = return . formatGL . maxFun . read