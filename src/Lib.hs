module Lib
    ( someFunc
    ) where

import Week10.Solution

someFunc :: IO ()
someFunc =
    print $ runParser intPair "12, 45"
