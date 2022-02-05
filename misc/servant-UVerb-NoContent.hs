#!/usr/bin/env cabal
{- cabal:
build-depends:
  , base
  , servant
  , servant-client
  , servant-client-core
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
-- this option is useful for seeing why we end up with the FromJSON constraint
-- {-# OPTIONS_GHC -ddump-cs-trace #-}

module Main where

import Data.Proxy
import Servant.API
import Servant.Client
import Servant.Client.Core

-- | This doesn't work, replacing @NoContent@ with @WithStatus 204 ()@ works
-- fine but sends a few extra bytes over the wire because it
-- serializes/deserializes @()@.
type Api = "test" :> UVerb 'POST '[JSON] '[WithStatus 400 Int, NoContent]

client :: Client ClientM Api
client = clientIn (Proxy :: Proxy Api) (Proxy :: Proxy ClientM)

main :: IO ()
main = error "can't run this; it only exists to be compiled"
