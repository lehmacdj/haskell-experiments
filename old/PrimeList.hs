module PrimeList where

import Primes

main :: IO ()
main = sequence_ $ (putStrLn . (++ " is prime.") . show) <$> takeWhile (<1000000) primes
