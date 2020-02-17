{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Week7.Scrabble where

import Data.Monoid
import Data.Char

newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

scoreChar :: Char -> Score
scoreChar c
  | c' `elem` "aeilnorstu" = Score 1
  | c' `elem` "dg"         = Score 2
  | c' `elem` "bcmp"       = Score 3
  | c' `elem` "fhvwy"      = Score 4
  | c' `elem` "k"          = Score 5
  | c' `elem` "jx"         = Score 8
  | c' `elem` "qz"         = Score 10
  | otherwise              = Score 0
    where c' = toLower c

instance Semigroup Score where
    (<>) = (+)

instance Monoid Score where
    mempty  = Score 0
    mappend = (+)
    
class Scored a where
    score :: a -> Score

instance Scored Score where
    score = id

instance Scored a => Scored (a,b) where
    score = score . fst
