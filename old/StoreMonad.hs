{-# LANGUAGE DeriveFunctor #-}

module StoreMonad where

import Control.Comonad
import Data.Semigroup

data Store s a = Store
  { peek :: s -> a
  , pos :: s
  } deriving Functor

instance Comonad (Store s) where
  extract (Store f s) = f s
  duplicate (Store f s) = Store (Store f) s

instance
 (Semigroup s, Monoid s) =>
 Applicative (Store s) where
  pure a = Store (const a) mempty
  Store f s <*> Store g t = Store (\m -> f m (g m)) (mappend s t)

-- This instance is illegal!
-- instance
--  Semigroup s =>
--  ComonadApply (Store s) where
--   Store f s <@> Store g t = Store (\m -> f m (g m)) (s <> t)

instance
 (Semigroup s, Monoid s) =>
 Monad (Store s) where
  return = pure
  m >>= k = Store
    (\s -> peek (k (peek m s)) s)
    (pos m `mappend` pos (k (peek m mempty)))
