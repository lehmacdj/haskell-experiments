#!/usr/bin/env stack
{- stack ghci
    --resolver lts-18.14
    --package monad-control
    --package transformers
    --package mtl
 -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.Reader.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader

-- * this is trivial to do

newtype WrapperT' m a = WrapperT' {unWrapperT' :: ReaderT Int m a}
  deriving (Functor, Applicative, Monad, MonadReader Int)

instance MonadTrans WrapperT' where
  lift = WrapperT' . lift

instance MonadTransControl WrapperT' where
  type StT WrapperT' a = a
  liftWith f = WrapperT' $ liftWith $ \run -> f (run . unWrapperT')
  restoreT = defaultRestoreT WrapperT'

-- * this is seemingly impossible to do

newtype WrapperT m a = WrapperT {unWrapperT :: ReaderT (m ()) m a}
  deriving (Functor, Applicative, Monad, MonadReader (m ()))

instance MonadTrans WrapperT where
  lift = WrapperT . lift

_magic :: a
_magic = error "impossible to define I think"

instance MonadTransControl WrapperT where
  type StT WrapperT a = a
  liftWith f = WrapperT $ ReaderT $ \action -> f (flip runReaderT (_magic action) . unWrapperT)
  restoreT = defaultRestoreT WrapperT

main :: IO ()
main = undefined
