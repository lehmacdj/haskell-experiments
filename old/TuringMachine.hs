module TuringMachine where

import Control.Comonad
import Data.Stream.Infinite

data Zipper a = Zipper (Stream a) a (Stream a)

left :: Zipper a -> Zipper a
left (Zipper (l :> ls) x rs) = Zipper ls l (x :> rs)

right :: Zipper a -> Zipper a
right (Zipper ls x (r :> rs)) = Zipper (x :> ls) r rs

blank :: a -> Zipper a
blank x = Zipper (pure x) x (pure x)

data Direction = L | R

-- only one element should be a Left
newtype Config a = Config [Either Integer a]

data TM a = TM
    { tape :: Zipper (Maybe a)
    , state :: Integer
    , delta :: Integer -> Maybe a -> Maybe (Direction, a)
    , final :: Integer -> Bool
    }
