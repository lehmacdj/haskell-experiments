#!/usr/bin/env stack
{- stack ghci
    --resolver lts-18.18
    --package random
    --package classy-prelude
 -}

{-# LANGUAGE NoImplicitPrelude #-}

import ClassyPrelude
import Data.Function ((&))
import System.Random
import qualified Prelude as P

letterAtPos :: Maybe Char -> Int -> String -> Bool
letterAtPos Nothing _ _ = True
letterAtPos (Just c) ix w = (w `indexEx` ix) == c

getWords ::
  [Char] ->
  [Char] ->
  Maybe Char ->
  Maybe Char ->
  Maybe Char ->
  Maybe Char ->
  Maybe Char ->
  IO [String]
getWords must can't _0 _1 _2 _3 _4 = do
  fives <- lines <$> P.readFile "./5-letter-words.txt"
  pure $
    fives
      & filter (\w -> all (`elem` w) must)
      & filter (\w -> not $ any (`elem` w) can't)
      & filter (letterAtPos _0 0)
      & filter (letterAtPos _1 1)
      & filter (letterAtPos _2 2)
      & filter (letterAtPos _3 3)
      & filter (letterAtPos _4 4)

getWord ::
  [Char] ->
  [Char] ->
  Maybe Char ->
  Maybe Char ->
  Maybe Char ->
  Maybe Char ->
  Maybe Char ->
  IO String
getWord must can't _0 _1 _2 _3 _4 = do
  valid <- getWords must can't _0 _1 _2 _3 _4
  ix <- randomRIO (0, length valid - 1)
  pure $ valid `indexEx` ix
