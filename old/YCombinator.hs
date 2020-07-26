module YCombinator where

fix :: (t -> t) -> t
fix f = f (fix f)

newtype Mu a = Mu (Mu a -> a)

yComb :: (b -> b) -> b
yComb f = (\h -> h $ Mu h) (\x -> f . (\(Mu g) -> g) x $ x)

yComb' :: (b -> b) -> b
yComb' f = (\h -> h $ Mu h) (\x -> f . (\(Mu g) -> g) x $ x)

factStep :: (Num p, Eq p) => (p -> p) -> p -> p
factStep f x = if x == 0 then 1 else x * f (x - 1)

fact :: Integer -> Integer
fact = yComb factStep

fact2 :: Integer -> Integer
fact2 = fix factStep

main :: IO ()
main = print $ fact2 6 + fact 6
