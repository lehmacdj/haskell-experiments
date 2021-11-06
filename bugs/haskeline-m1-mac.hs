#!/usr/bin/env cabal
{- cabal:
build-depends:
  , base ==4.14.3.0
  -- ^ for pinning ghc-8.10.7
  , haskeline ==0.8.2
-}

-- | Simple example program for demo-ing problems with haskeline line editing on
-- M1 mac.
module Main where

import System.Console.Haskeline

repl :: InputT IO ()
repl = do
  mline <- getInputLine "> "
  case mline of
    Nothing -> pure ()
    Just line -> outputStrLn line *> repl

main :: IO ()
main = runInputT defaultSettings repl
