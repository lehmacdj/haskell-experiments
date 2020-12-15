#!/usr/bin/env stack
{- stack
  exec ghci
  --resolver lts-16.25
-}

{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Trying to figure out how default classes work for potential improvement to
-- mono-traversable
module Main (main) where

-- | Default signatures allow only a single implementation not two different
class Test a where
  frobnicate :: a -> a
  default frobnicate :: Num a => a -> a
  frobnicate = (+ 1)

instance Test Int where
  frobnicate = (+ 2)

main :: IO ()
main = undefined
