{-# LANGUAGE ViewPatterns #-}

module NetKAT where

import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad

data NetKAT a = Process (a -> Maybe a)
              | NetKAT a :+ NetKAT a
              | NetKAT a :. NetKAT a
              | Star (NetKAT a)
              | Dup

-- denotation of NetKAT language into Haskell
interpret :: NetKAT a -> NonEmpty a -> [NonEmpty a]
interpret (Process f) ((f -> Just x) :| h) = [x :| h]
interpret (Process _) _ = []
interpret (p :+ q) h = interpret p h ++ interpret q h
interpret (p :. q) h = (interpret p >=> interpret q) h
interpret (Star p) h = concat $ iterate (\i -> interpret p >=> i) pure <*> [h]
interpret Dup (x :| h) = [x :| x : h]
