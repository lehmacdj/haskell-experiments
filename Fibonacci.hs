module Fibonacci where
import Data.Function

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- https://wiki.haskell.org/Memoization
fib f 0 = 0
fib f 1 = 1
fib f n = f (n - 1) + f (n - 2)
memoize f = (map f [0..] !!)
fibonacci = fix (memoize . fib)
