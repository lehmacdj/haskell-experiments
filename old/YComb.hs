module Main where

newtype Mu a = Mu (Mu a -> a)

yComb :: (b -> b) -> b
yComb f = (\h -> h $ Mu h) (\x -> f . (\(Mu g) -> g) x $ x)

factStep :: (Num p, Eq p) => (p -> p) -> p -> p
factStep f x = if x == 0 then 1 else x * f (x - 1)

factStep' :: (Num p, Eq p) => (p -> p) -> p -> p
factStep' _ 0 = 1
factStep' f x = f (x - 1) * x

countStep :: (Num p, Eq p) => (p -> p) -> p -> p
countStep f x = if x == 0 then 1 else 1 + f (x - 1)

fact :: Integer -> Integer
fact = yComb factStep'

main :: IO ()
main = print $ fact 2
