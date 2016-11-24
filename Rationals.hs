module Rationals where

data Rational = Rational Integer Integer

gcd :: Integer -> Integer -> Integer
gcd a 0 = a
gcd a b = gcd b (a `mod` b)

simplify :: Rational -> Rational
simplify (Rational p q) =
    Rational (p / l) (q / l)
    where
    l = gcd p q

addRat :: Rational -> Rational -> Rational
addRat (Rational p1 q1) (Rational p2 q2) =
    Rational (p1 * q2 + p2 * q1) (q1 * q2)

multiplyRat :: Rational -> Rational -> Rational
multiplyRat (Rational p1 q1) (Rational p2 q2) =
    Rational (p1 * p2) (q1 * q2)

negateRat :: Rational -> Rational
negateRat (Rational p q) = Rational (negate p) q

absRat :: Rational -> Rational
absRat (Rational p q) = Rational (abs p) q

signRat :: Rational -> Rational
signRat (Rational p q) = Rational (signum p) 1

fromIntRat :: Integer -> Rational
fromIntRat i = Rational i 1

instance Num Rational where
    (+) = addRat
    (*) = multiplyRat
    negate = negateRat
    abs = absRat
    signum = signumRat
    fromInteger = fromIntegerRat

instance Eq Rational where
    (Rational p1 q1) == (Rational p2 q2) = p1 * q2 == p2 * q1
