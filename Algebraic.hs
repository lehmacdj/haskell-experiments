module Rational
( AlgebraicNumber
) where

import Data.Ratio

newtype AlgebraicNumber = ANum [Rational]

-- Details on possible implementation details here
-- http://reduce-algebra.com/docs/arnum.pdf
instance Num AlgebraicNumber where
    -- Add all terms except for the first
    x + y = error "unimplemented"
    -- Multiply terms and then reduce
    x * y = error "unimplemented"
    negate x = error "unimplemented"
    abs x = error "unimplemented"
    signum x = error "unimplemented"
    fromInteger i = [Term 1 1, Term 0 i]

instance Eq AlgebraicNumber where
    (==) = error "unimplemented"

instance Ord AlgebraicNumber where
    (>) = error "unimplemented"
