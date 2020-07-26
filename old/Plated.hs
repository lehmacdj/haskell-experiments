{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module Plated where

import Data.Data
import Control.Monad.State
import Control.Lens.Plated
import Control.Comonad.Store

data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Neg Expr
          deriving (Show, Eq, Data, Typeable)

instance Plated Expr

constants :: Expr -> [Int]
constants e = [c | Val c <- universe e]

dividesByZero :: Expr -> Bool
dividesByZero e = any zeroDivision (universe e)
    where zeroDivision (Div _ (Val 0)) = True
          zeroDivision _ = False

optimize :: Expr -> Expr
optimize = transform f
    where f (Neg (Val i)) = Val (negate i)
          f (Add x y) | x == y = Mul x (Val 2)
          f (Add x (Mul y (Val 2))) | x == y = Mul x (Val 3)
          f e = e

depth :: Expr -> Int
depth = para (\_ cs -> 1 + maximum (0:cs))

nestedAdditions :: Expr -> Int
nestedAdditions = para incForAdd
    where incForAdd (Add _ _) cs = 1 + maximum (0:cs)
          incForAdd _ cs = maximum (0:cs)

uniqLits :: Expr -> Expr
uniqLits e = evalState (transformM f e) [0..]
    where
        f (Val i) = do
            (y:ys) <- get
            put ys
            return (Val y)
        f x = return x

noDups :: Expr -> Expr
noDups e = evalState (transformM f e) []
    where
        f (Val i) = do
            ys <- get
            if i `elem` ys
            then return (Val 0)
            else do
                put (i:ys)
                return (Val i)
        f x = return x

mutate :: Expr -> [Expr]
mutate x = experiment f =<< contexts x where
    f (Val i) = [Val (i + 1), Val (i - 1)]
    f e = pure e

swapMultForAdd :: Expr -> Expr
swapMultForAdd x = head $ experiment f =<< contexts x where
    f (Mul a b) = [Add a b]
    f e = pure e
