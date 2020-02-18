module Week8.Solution where

import Week8.Employee

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps funs) = GL (emp:emps) (empFun emp + funs)

instance Semigroup GuestList where
    (<>) (GL e1 f1) (GL e2 f2) = GL (e1 ++ e2) (f1 +f2) 

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL e1 f1) (GL e2 f2) = GL (e1 ++ e2) (f1 +f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

test1 = do
  print $ glCons e mempty <> gl
  print $ moreFun gl (glCons e mempty)
    where
      e = Emp "Sharon" 60
      gl = GL [Emp "Frank" 47, Emp "Mary" 15 ] 62