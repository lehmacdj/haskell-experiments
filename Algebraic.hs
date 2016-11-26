module Rational
( AlgebraicNumber
) where

import Data.Ratio

-- An algebraic number is a list of the coefficients of
-- 1 + x + x^2 + x^3 + x^4 + ...
-- if a term is missing it is implicitly 0
-- except for the term just after the last term which is implicitly 1
newtype AlgebraicNumber = ANum { getANum :: [Rational] }
    deriving (Show)

-- make the length be as small as possible
cannonicalize :: AlgebraicNumber -> AlgebraicNumber
cannonicalize = ANum . reverse . dropWhile (==0) . reverse . getANum

-- make and cannonicalize
makeANum :: [Rational] -> AlgebraicNumber
makeANum = cannonicalize . ANum

listWithLen :: AlgebraicNumber -> Int -> [Rational]
listWithLen (ANum list) len
    | length list >= len  = list
    | length list < len   = listWithLen (ANum list) len

-- Details on possible implementation details here
-- http://reduce-algebra.com/docs/arnum.pdf
instance Num AlgebraicNumber where
    -- Add all terms with one another
    x + y =
        let x' = listWithLen x (length $ getANum y)
            y' = listWithLen y (length x')
        in  makeANum $ zipWith (+) x' y'
    -- Multiply terms and then reduce
    x * y =
        let x' = listWithLen x (length $ getANum y)
            y' = listWithLen y (length x')
        in  makeANum $ zipWith (*) x' y'
    negate (ANum x) = makeANum $ map negate x
    abs x = undefined
    signum x = undefined
    fromInteger i = ANum [-i % 1]

instance Eq AlgebraicNumber where
    (==) = undefined

instance Ord AlgebraicNumber where
    compare = undefined
