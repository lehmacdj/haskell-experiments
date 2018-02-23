{-# LANGUAGE RankNTypes #-}

module LensExtras where

import Control.Lens
import Control.Monad

-- * Lens helpers

-- | Traversal that can use monadic effects. Note that while this is a Setter it
-- fails to be a getter because `Const` is not a monad but is an applicative,
-- so lens combinators like `view` cannot be used on a MTraversal.
-- MTraversal laws are the same as the Traversal laws.
--
-- Every MTraversal is also a Traversal and a Setter and thus can be used in
-- contexts where one of those is expected instead.
type MTraversal s t a b = forall m. Monad m => (a -> m b) -> s -> m t
type MTraversal' s a = Simple MTraversal s a

-- | Kleisli Traversal composition. Result is a MTraversal' s a that traverses
-- over the targets in both of the arguments. Note that this function is
-- unsound in general as it violates the Traversal laws. It is therefore
-- required as a precondition that the two Traversal traverse disjoint parts of
-- s.
--
-- The order that monadic effects are applied in is left unspecified, because
-- it doesn't matter if the precondition of this function is maintained.
traverseBoth :: Traversal' s a -> Traversal' s a -> MTraversal' s a
traverseBoth t1 t2 f x = join (t1 f <$> t2 f x)
