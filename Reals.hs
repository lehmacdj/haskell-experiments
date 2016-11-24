{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module ConstructiveReal
( CReal
, evaluate
) where

import Text.Printf (printf)
import Data.Ratio
import qualified Data.List as L

-- This is an implementation of the real number type from bishops book
-- for some real number r represented by x:
--      (x n) + 1/10^n > r > (x n) - 1/10^n
-- i.e. for n; n decimal places in (x n) are accurate; this means that any
-- real number can be evaluated as accurately as desired
type CReal = Integer -> Rational

cannonicalBound :: CReal -> Integer
cannonicalBound x = 2 + (abs $ ceiling (x 1))

instance Num CReal where
    x + y = \n -> (x (2 * n)) + (y (2 * n))
    x * y =
        let k = max (cannonicalBound x) (cannonicalBound y)
        in  \n -> (x (2 * k * n)) * (y (2 * k * n))
    negate x = negate . x
    abs x = abs . x
    signum x = signum . x
    fromInteger i = \n -> i % 1

-- Find N such that |x N| > 1/N
inverseBound :: CReal -> Integer
inverseBound x =
    -- Test if x is greater than 1 with accuracy n
    -- a/b > 1/n -> a * n > b
    let gtInvAccuracy n = a * n > b
            where a = numerator (x n)
                  b = denominator (x n)
    in  maybe 0 id $ L.find gtInvAccuracy [1..]

instance Fractional CReal where
    recip x =
        let bound = inverseBound x
        in  \n ->
            if n < bound
                then recip (x $ bound ^ 3)
                else recip (x $ bound ^ 2)
    fromRational r = \n -> r

instance Show CReal where
    -- print with accuracy 10 ^ -5
    show x = showReal x 5

evaluate :: CReal -> Integer -> Rational
evaluate x = \n -> x n

showReal :: CReal -> Integer -> String
showReal x n = printf ("%." ++ show n ++ "f") (fromRational $ x n :: Double)

instance Floating CReal where
    -- Series expansion that gives us this number
    pi = error "unimplemented"
    -- Evaluated using a taylor expansion -- not sure if this conforms to the
    -- strict definition set by bishop: it may be too small etc
    -- calculate until error is less than 10 ^ n
    exp x = \n ->
        let cal_y_n x_n n =
                let helper sum x_term fact k =
                        if k > n
                            then sum
                            else let x_term' = x_n * x_term
                                     fact'   = k * fact
                                     sum'    = sum + x_term' / fact'
                                     k'      = k + 1
                                 in  helper sum' x_term' fact' k'
                in  helper 1 1 1 1
        in  cal_y_n (x $ n + 3) (3 + fromIntegral n)
    log = error "unimplemented"
    sin = error "unimplemented"
    cos = error "unimplemented"
    asin = error "unimplemented"
    acos = error "unimplemented"
    atan = error "unimplemented"
    sinh = error "unimplemented"
    cosh = error "unimplemented"
    asinh = error "unimplemented"
    acosh = error "unimplemented"
    atanh = error "unimplemented"
