{-# LANGUAGE DeriveFunctor #-}

{-|
   Computations that allow dealing with past values.
 -}
module Coeffects where

import Control.Comonad

-- | A comonad that represents a computation that can keep track of past values.
-- Laws:
-- > update f = extract . fmap (update f) . duplicate
-- > undo = fmap extract . undo . duplicate
-- > undo . update f = Just
class Comonad w => ComonadHistory w where
  -- | Update the current value, recording the previous value in history.
  update :: (a -> a) -> w a -> w a
  -- | Get the last value of the computation or nothing if there was no
  -- previous computation.
  undo :: w a -> Maybe (w a)

data History a = History [a] a
  deriving (Functor, Show, Eq)

instance Comonad History where
  extract (History _ x) = x
  duplicate (History h' x) = History (mkHist h') (History h' x) where
    mkHist [] = []
    mkHist (h:hs) = History hs h : mkHist hs

instance ComonadHistory History where
  update f (History h x) = History (x : h) (f x)
  undo (History [] _) = Nothing
  undo (History (h:hs) _) = Just $ History hs h

ex0 :: History Int
ex0 = History [1, 2] 0
