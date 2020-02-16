{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Week7.Scrabble where

import Data.Monoid

newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Semigroup Score where
    (<>) = (+)

instance Monoid Score where
    mempty  = Score 0
    mappend = (+)
    
class Scored a where
    score :: a -> Score

instance Scored Score where
    score = id

instance Scored b => Scored (a,b) where
    score = score . snd
