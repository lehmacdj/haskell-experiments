fix f = f (fix f)

newtype Mu a = Mu (Mu a -> a)
yComb f = (\h -> h $ Mu h) (\x -> f . (\(Mu g) -> g) x $ x)

factStep f x = if x == 0 then 1 else x * f (x - 1)

fact = yComb factStep

fact2 = fix factStep

main = print $ (fact2 6) + (fact 6)
