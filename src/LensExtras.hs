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

-- Copied from lens-4.16.1, because we have an older version of lens
-- | A lens product. There is no law-abiding way to do this in general.
-- Result is only a valid 'Lens' if the input lenses project disjoint parts of
-- the structure @s@. Otherwise "you get what you put in" law
--
-- @
-- 'Control.Lens.Getter.view' l ('Control.Lens.Setter.set' l v s) ≡ v
-- @
--
-- is violated by
--
-- >>> let badLens :: Lens' (Int, Char) (Int, Int); badLens = lensProduct _1 _1
-- >>> view badLens (set badLens (1,2) (3,'x'))
-- (2,2)
--
-- but we should get @(1,2)@.
--
-- Are you looking for 'Control.Lens.Lens.alongside'?
--
lensProduct :: ALens' s a -> ALens' s b -> Lens' s (a, b)
lensProduct l1 l2 f s =
    f (s ^# l1, s ^# l2) <&> \(a, b) -> s & l1 #~ a & l2 #~ b

-- Copied from lens-4.16.1, because we have an older version of lens
-- | A dual of `lensProduct`: a prism sum.
--
-- The law
--
-- @
-- 'Control.Lens.Fold.preview' l ('Control.Lens.Review.review' l b) ≡ 'Just' b
-- @
--
-- breaks with
--
-- >>> let badPrism :: Prism' (Maybe Char) (Either Char Char); badPrism = prismSum _Just _Just
-- >>> preview badPrism (review badPrism (Right 'x'))
-- Just (Left 'x')
--
-- We put in 'Right' value, but get back 'Left'.
--
-- Are you looking for 'Control.Lens.Prism.without'?
--
prismSum :: APrism s t a b
         -> APrism s t c d
         -> Prism s t (Either a c) (Either b d)
prismSum k =
    withPrism k                  $ \bt seta k' ->
    withPrism k'                 $ \dt setb    ->
    prism (either bt dt) $ \s ->
    f (Left <$> seta s) (Right <$> setb s)
  where
    f a@(Right _) _ = a
    f (Left _)    b = b

-- | Cons APrism onto a traversal. The prism is tried first, if it hits, the
-- traversal is skipped, otherwise the traversal is used.
--
-- Example usage:
-- @
-- λ> over (mapped . (_Left `prismCons` _Right)) (+1) [Right 1, Left 2, Right 3]
-- [Right 2,Left 3,Right 4]
-- @
--
-- Derived from @prismSum@ in Control.Lens.Unsound, however this combinator is
-- actually sound because the resulting traversal is a valid traversal
-- regardless of which branch is taken.
--
-- TODO: consider if this is worth trying to get into lens
prismCons :: APrism s t a b
          -> Traversal s t a b
          -> Traversal s t a b
prismCons k =
  withPrism k $ \_ seta tr f s ->
  pick (seta s) tr f s
  where
    pick (Right _) _ f s = clonePrism k f s
    pick (Left _)  tr f s = tr f s

-- | Lazily try 2 traversals. If the first one fails to target any elements
-- the second one is tried instead. Less efficient and less flexible than
-- @prismCons@.
traversalOr :: Traversal' s a -> Traversal' s a -> Traversal' s a
traversalOr tr1 tr2 f s
  | null (toListOf tr1 s) = tr2 f s
  | otherwise = tr1 f s

infixr 8 |:
infixr 7 ||:

(|:) :: APrism s t a b -> Traversal s t a b -> Traversal s t a b
(|:) = prismCons

(||:) :: Traversal' s a -> Traversal' s a -> Traversal' s a
(||:) = traversalOr
