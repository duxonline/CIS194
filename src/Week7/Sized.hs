{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Week7.Sized where

import Data.Monoid

newtype Size = Size Int
  deriving (Eq, Ord, Show, Num)

getSize :: Size -> Int
getSize (Size i) = i

instance Semigroup Size where
  (<>) = (+)

instance Monoid Size where
  mempty  = Size 0
  mappend = (+)

class Sized a where
  size :: a -> Size

instance Sized Size where
  size = id

-- The example are (Foo, Size) and (Foo, (Bar, Size))
instance Sized b => Sized (a,b) where
  size = size . snd
