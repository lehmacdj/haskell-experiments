{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Regex where

import Prelude hiding (null)
import Data.Functor.Foldable

data RegexF n a = StarF n
                | VoidF
                | CharF a
                | n :. n
                | n :+ n
                | EpsilonF
                deriving (Functor)

type Regex a = Fix (RegexF a)

-- null :: Regex a -> Regex a
-- null = cata $ \case
--     StarF n -> EpsilonF
--     VoidF -> VoidF
--     CharF c -> VoidF
--     s :. t -> null s :. null t
--     s :+ t -> null s :+ null t
--     EpsilonF -> EpsilonF

deriv :: Eq a => Regex a -> [a] -> Regex a
deriv = cata $ \case
    StarF _ -> undefined
    VoidF -> undefined
    CharF _ -> undefined
    _ :. _ -> undefined
    _ :+ _ -> undefined
    EpsilonF -> undefined

match :: Eq a => [a] -> Regex a -> Bool
match = undefined
