{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module RecursionSchemes where

import Test.QuickCheck
import Data.Functor.Foldable hiding (ListF)
import Control.Applicative (liftA2)
import Control.Arrow
import Data.Monoid

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

data ListF a n = NilF
               | ConsF a n
               deriving (Functor)

type List a = Fix (ListF a)

mconcatl :: Monoid m => List m -> m
mconcatl = cata alg where
    alg NilF = mempty
    alg (ConsF m s) = m `mappend` s

-- implementation of fix in terms of hylo to prove turing completeness of hylo
fixl :: (a -> a) -> a
fixl = hylo alg coalg where
    alg NilF = undefined -- this never occurs
    alg (ConsF f fx) = f fx
    coalg f = ConsF f f

fromList :: [a] -> List a
fromList = ana coalg where
    coalg [] = NilF
    coalg (h:t) = ConsF h t

toList :: List a -> [a]
toList = cata alg where
    alg NilF = []
    alg (ConsF h t) = h:t

-- pointfree testing nonsense
compareFunc :: (Arbitrary a, Eq b) => (a -> (b, b)) -> a -> Bool
compareFunc = (uncurry (==) .)

extensional :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
extensional = liftA2 (==)

isIdentity :: (Arbitrary a, Eq a) => (a -> a) -> a -> Bool
isIdentity = compareFunc . (&&& id)

isIso :: (Arbitrary a, Eq a) => (a -> b) -> (b -> a) -> a -> Bool
isIso = (isIdentity .) . flip (.)

prop_natConvert :: Positive Integer -> Bool
prop_natConvert = isIso fromInt toInt . getPositive

prop_listConvert :: [Integer] -> Bool
prop_listConvert = isIso fromList toList

prop_mconcatl :: [Integer] -> Bool
prop_mconcatl = extensional mconcat (mconcatl . fromList) . fmap Sum
