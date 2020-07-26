module Decorations where

-- | Wrap an ordered thing with an unordered thing using the ordering of the
-- first thing, to get an ordering for the whole thing.
data OrdDecoration a b = OrdDec a b
  deriving Show
instance Eq a => Eq (OrdDecoration a b) where
  OrdDec a1 _ == OrdDec a2 _ = a1 == a2
instance Ord a => Ord (OrdDecoration a b) where
  compare (OrdDec a1 _) (OrdDec a2 _) = compare a1 a2

-- | Get the unordered element of a decorated element.
undecorated :: OrdDecoration a b -> b
undecorated (OrdDec _ b) = b

-- | Decorate a thing, by extracting a way of ordering it.
decorate :: Ord a => (b -> a) -> b -> OrdDecoration a b
decorate f a = OrdDec (f a) a
