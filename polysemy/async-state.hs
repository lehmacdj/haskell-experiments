#!/usr/bin/env stack
{- stack
  exec ghci
  --resolver lts-16.6
  --package polysemy
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Module for testing out the way that async effects are evaluated with
-- respect to atomic state effects.
module Main (main) where

import Polysemy
import Polysemy.Async
import Polysemy.AtomicState

test1 ::
  Members [AtomicState s, Async] r =>
  Sem r ()
test1 = undefined

runStateThenAsync ::
  Member (Embed IO) r => s -> Sem (AtomicState s : Async : r) a -> Sem r (s, a)
runStateThenAsync s = asyncToIO . atomicStateToIO s

runAsyncThenState ::
  Member (Embed IO) r => s -> Sem (Async : AtomicState s : r) a -> Sem r (s, a)
runAsyncThenState s = atomicStateToIO s . asyncToIO

main :: IO ()
main = undefined
