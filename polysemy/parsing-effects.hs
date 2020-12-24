#!/usr/bin/env stack
{- stack
  exec ghci
  --resolver lts-16.26
  --package polysemy
  --package megaparsec
  --package classy-prelude
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Experiment for seeing if we can write parsers that generate effects
-- directly, to provide a decoupled architecture for implementing commands
--
-- Conclusion: this works pretty well, but there are a couple of
-- annoyances/surprising benefits:
-- * There isn't a way to guarantee that all of the effects are parsed. For
-- example we could have redefined parser in parseEffects as just 'parseA' and
-- then we would silently fail to parse b's but this wouldn't be reflected at
-- all. We need a way to require parsers to be implemented. This probably
-- requires a co-effect system of some kind. With the requirement that there
-- be a parser for each effect expressed by some kind of distributive law.
-- * We do however maintain completeness for implementation of the effects. We
-- wouldn't be able to compile the program if we didn't handle every effect
-- that we generate.
-- * The different parsers were able to be combined fairly smoothly
module Main (main) where

import ClassyPrelude hiding (try)
import Data.Void
import Polysemy
import Polysemy.Trace (Trace)
import qualified Polysemy.Trace as T
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

data A m a where
  A :: A m ()

makeSem ''A

parseA :: Member A r => Parser (Sem r ())
parseA = string "a" $> a

runATrace :: Member Trace r => Sem (A : r) a -> Sem r a
runATrace = interpret (\A -> T.trace "received a")

data B m a where
  B :: B m ()

makeSem ''B

parseB :: Member B r => Parser (Sem r ())
parseB = string "b" $> b

runBTrace :: Member Trace r => Sem (B : r) a -> Sem r a
runBTrace = interpret (\B -> T.trace "received b")

runEffects :: Sem [A, B, Trace, Embed IO] () -> IO ()
runEffects = runM . T.traceToIO . runBTrace . runATrace

parseEffects :: Members [A, B, Trace] r => Text -> Sem r ()
parseEffects input =
  let parser = (try parseA <|> parseB) <* eof
   in case parse parser "<interactive>" input of
        Right e -> e
        Left err -> T.trace (show err)

main :: IO ()
main = forever $ do
  line <- getLine
  runEffects . parseEffects $ line
