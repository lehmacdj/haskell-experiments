module Fibonacci where
import Data.Function
import Data.List (genericIndex)

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- https://wiki.haskell.org/Memoization
fib :: (Num t, Num a, Eq a) => (a -> t) -> a -> t
fib _ 0 = 0
fib _ 1 = 1
fib f n = f (n - 1) + f (n - 2)

memoize :: (Num a, Enum a) => (a -> b) -> Int -> b
memoize f = (map f [0..] `genericIndex`)

fibonacci :: Int -> Integer
fibonacci = fix (memoize . fib)
