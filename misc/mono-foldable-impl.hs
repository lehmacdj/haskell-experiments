#!/usr/bin/env stack
{- stack
  exec ghci
  --resolver lts-16.25
  --package mono-traversable
  --package bytestring
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

-- | Repro of compile error for this issue:
-- https://github.com/snoyberg/mono-traversable/issues/191
module Main (main) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Data.MonoTraversable
import Data.Word

data Test = Test {_a :: Int, _b :: Int}

type instance Element Test = Int

-- the trick is that each default-ed method needs an implementation, otherwise
-- this works relatively normally
instance MonoFoldable Test where
  -- ofoldMap :: Monoid m => (Int -> m) -> Test -> m
  ofoldMap f (Test a b) = f a <> f b

  -- ofoldr :: (Int -> a -> a) -> a -> Test -> a
  ofoldr f acc (Test a b) = f a (f b acc)

  -- ofoldl' :: (a -> Int -> a) -> a -> Test -> a
  ofoldl' f acc (Test a b) = f (f acc a) b

  ofoldr1Ex f (Test a b) = f a b

  ofoldl1Ex' f (Test a b) = f a b

newtype Test2 = Test2 {unTest2 :: ByteString}

type instance Element Test2 = Word8

-- instance MonoFoldable Test2 where
--   ofoldMap f = ofoldr (mappend . f) mempty
--   ofoldr f acc = S.foldr f acc . unTest2
--   ofoldl' f acc = S.foldl' f acc . unTest2
--   otoList = S.unpack . unTest2
--   oall f = S.all f . unTest2
--   oany f = S.any f . unTest2
--   onull = S.null . unTest2
--   olength = S.length . unTest2
--   oelem e = S.elem e . unTest2
--   onotElem e = S.notElem e . unTest2

main :: IO ()
main = error "only intended for use in interactive ghci session"
