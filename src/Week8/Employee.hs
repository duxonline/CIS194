module Week8.Employee where

import Data.Tree

type Name = String

type Fun = Integer

data Employee = Emp { empName :: Name, empFun :: Fun}
    deriving (Show, Read, Eq)

testCompany :: Tree Employee
testCompany
  = Node (Emp "Stan" 9)
    [ Node (Emp "Bob" 2)
      [ Node (Emp "Joe" 5)
        [ Node (Emp "John" 1) []
        , Node (Emp "Sue" 5) []
        ]
      , Node (Emp "Fred" 3) []
      ]
    , Node (Emp "Sarah" 17)
      [ Node (Emp "Sam" 4) []
      ]
    ]

data GuestList = GuestList [Employee] Fun
    deriving (Show, Eq)

instance Ord GuestList where
    compare (GuestList _ f1) (GuestList _ f2) = compare f1 f2