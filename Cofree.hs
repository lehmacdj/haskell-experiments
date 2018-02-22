module Cofree where

import Control.Comonad.Cofree

-- model asts using stuff
data A' a b = A a b
newtype B' a = B a

type B a = Cofree B' a
type A a = Cofree (A' (B a)) a

-- is it possible to write a function:
-- erase :: A a -> A ()
-- and how is it done
