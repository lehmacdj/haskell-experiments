module PollardRho where

import Data.Maybe (mapMaybe)
import Data.Bifunctor

number :: Integer
number = 115792089237316195423570985008687907853269984665640564039457584007913129639937

otherNumber :: Integer
otherNumber = 1287213871381123

gd :: Integral a => a -> a -> a
gd n x = (x * x + 1) `mod` n

pollardRho :: Integer -> (Integer -> Integer -> Integer) -> Integer -> Maybe Integer
pollardRho n g x = if d == n then Nothing else Just d where
    d = head $ dropWhile (==1) pfactors
    xs = iterate (g n) x
    everyOther (y:_:ys) = y:everyOther ys
    everyOther _ = error "infinite lists only"
    xs2 = everyOther xs
    pfactors = drop 1 $ zipWith (((gcd n . abs) .) . (-)) xs xs2

fromLeft :: Either t1 t -> t1
fromLeft (Left x) = x
fromLeft _ = error "not left"

isRight :: Either t1 t -> Bool
isRight (Right _) = True
isRight _ = False

trialDivision :: Integer -> [Integer]
trialDivision n = go ([], n) [2..] where
    go (_, _) [] = error "somehow there was an empty list?"
    go (fs, n') (x:xs)
      | n' == 1 = fs
      | otherwise = go (first (fs++) $ tryDivide x n') xs
    tryDivide x =
        bimap (map $ const x) (fromLeft . head)
        . span isRight
        . drop 1
        . iterate (>>= (`divEvenly` x))
        . Right
    divEvenly a b =
        let (q, r) = a `divMod` b
         in if r == 0 then Right q else Left a

factorize :: Integer -> [Integer]
factorize n = fastFactorize n >>= trialDivision

fastFactorize :: Integer -> [Integer]
fastFactorize n | n <= 0 = error "no negative or zero numbers"
fastFactorize 1 = []
fastFactorize n = if null factors then [n] else factors where
    factors = (factor ++ map (n `div`) factor) >>= fastFactorize
    factor = take 1 results
    results = mapMaybe (pollardRho n g) [2..100]
    g n' x = (x * x + 1) `mod` n'
