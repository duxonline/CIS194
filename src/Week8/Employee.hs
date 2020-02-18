module Week8.Employee where

import Data.Tree

type Name = String

type Fun = Integer

data Employee = Emp { empName :: Name, empFun :: Fun}
    deriving (Show, Read, Eq)

testCompany :: Tree Employee
testCompany = undefined

data GuestList = GL [Employee] Fun
    deriving (Show, Eq)

instance Ord GuestList where
    compare (GL _ f1) (GL _ f2) = compare f1 f2