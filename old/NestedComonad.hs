{-# LANGUAGE DeriveFunctor, LambdaCase, TypeApplications, DeriveGeneric #-}

module NestedComonad where

import Control.Comonad
import Test.QuickCheck
import GHC.Generics

data A a = A' a | A a (A a)
    deriving (Functor, Show, Read, Eq, Generic)

data B a = B' a (B a) | B (A a) a
    deriving (Functor, Show, Read, Eq, Generic)

instance Comonad A where
    extract = \case
        A x _ -> x
        A' x -> x
    duplicate = \case
        a@(A _ xs) -> A a (duplicate xs)
        a@(A' _) -> A' a

instance Comonad B where
    extract = \case
        B' a _ -> a
        B _ a -> a
    duplicate = \case
        b@(B' _ b') -> B' b (duplicate b')
        b@(B as _) -> B (const b <$> as) b

instance Arbitrary a => Arbitrary (A a) where
    arbitrary = oneof
        [ A' <$> arbitrary
        , A' <$> arbitrary
        , A' <$> arbitrary
        , A <$> arbitrary <*> arbitrary
        ]
    shrink = \case
        A _ a -> [a]
        _ -> []

instance Arbitrary a => Arbitrary (B a) where
    arbitrary = oneof
        [ B' <$> arbitrary <*> arbitrary
        , B <$> arbitrary <*> arbitrary
        , B <$> arbitrary <*> arbitrary
        , B <$> arbitrary <*> arbitrary
        ]
    shrink = \case
        B' _ b -> [b]
        _ -> []

instance CoArbitrary a => CoArbitrary (A a)
instance Function a => Function (A a)
instance CoArbitrary a => CoArbitrary (B a)
instance Function a => Function (B a)

law1 :: (Comonad w, Show (w a), Eq (w a)) => w a -> Property
law1 x = extract (duplicate x) === x

law2 :: (Comonad w, Show (w a), Eq (w a)) => w a -> Property
law2 x = fmap extract (duplicate x) === x

law3 :: (Comonad w, Show (w (w (w a))), Eq (w (w (w a)))) => w a -> Property
law3 x = duplicate (duplicate x) === fmap duplicate (duplicate x)

test :: IO ()
test = do
    putStrLn "Tests for A"
    quickCheck $ law1 @A @()
    quickCheck $ law2 @A @()
    quickCheck $ law3 @A @()
    putStrLn "Tests for B"
    quickCheck $ law1 @B @()
    quickCheck $ law2 @B @()
    quickCheck $ law3 @B @()
