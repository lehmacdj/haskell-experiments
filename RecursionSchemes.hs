{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module RecursionSchemes where

import Test.QuickCheck
import Data.Functor.Foldable
import Control.Arrow

data NatF a = ZeroF | SuccF a
    deriving (Functor)

type Nat = Fix NatF

fromInt :: Integer -> Nat
fromInt = ana coalg where
    coalg n
      | n == 0 = ZeroF
      | otherwise = SuccF (n - 1)

pra :: (a -> a) -> a -> Nat -> a
pra f x = cata alg where
    alg ZeroF = x
    alg (SuccF k) = f k

toInt :: Nat -> Integer
toInt = pra (+1) 0

-- pointfree testing nonsense
compareFunc :: (Arbitrary a, Eq a) => (a -> (a, a)) -> a -> Bool
compareFunc = (uncurry (==) .)

isIdentity :: (Arbitrary a, Eq a) => (a -> a) -> a -> Bool
isIdentity = compareFunc . (&&& id)

isIso :: (Arbitrary a, Eq a) => (a -> b) -> (b -> a) -> a -> Bool
isIso = (isIdentity .) . flip (.)

prop_natConvert :: Positive Integer -> Bool
prop_natConvert = isIso fromInt toInt . getPositive
