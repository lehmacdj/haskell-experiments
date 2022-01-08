#!/usr/bin/env stack
{- stack ghci
    --resolver lts-18.18
    --package generic-lens
    --package lens
    --package random
    --package mtl
    --package classy-prelude
 -}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | This generates a gif that shows the probability of each camel to win
-- roundwise as a game of camel cup evolves.
module Main (main) where

import ClassyPrelude
import Control.Lens
import Control.Monad.State.Strict
import Data.Generics.Labels
-- import Diagrams.Prelude

import Data.Maybe
import Data.Tuple
import GHC.Generics (Generic)
import System.Random

data Camel
  = White
  | Orange
  | Blue
  | Yellow
  | Green
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | The camel cup board encoded as the position of each camel in addition to
-- the camels its above.
data Board = Board
  { white :: CamelPos,
    orange :: CamelPos,
    blue :: CamelPos,
    yellow :: CamelPos,
    green :: CamelPos
  }
  deriving (Show, Eq, Ord, Generic)

-- | Location 0 represents the first location a camel may
-- be (if they rolled a 1 before the game starts as their position is
-- initialized), the first location past the finish line is given by
-- 'finishLine'. camelsAbove represents the camels the camel is below. The
-- camels are presented in order: if the CamelPos of White is @CamelPos 3
-- [Blue, Orange]@ then the positions of Blue and Orange are @CamelPos 3
-- [Orange]@ and @CamelPos 3 []@ respectively and Orange is at the top of the
-- stack.
data CamelPos = CamelPos
  { location :: Int,
    camelsAbove :: [Camel]
  }
  deriving (Show, Eq, Ord, Generic)

camelPos :: Camel -> Lens' Board CamelPos
camelPos = \case
  White -> #white
  Orange -> #orange
  Blue -> #blue
  Yellow -> #yellow
  Green -> #green

camelPositions :: Board -> [(Camel, CamelPos)]
camelPositions Board {white, orange, blue, yellow, green} =
  [ (White, white),
    (Orange, orange),
    (Blue, blue),
    (Yellow, yellow),
    (Green, green)
  ]

camelsAtLocation :: Int -> Board -> [Camel]
camelsAtLocation p =
  map fst . filter (has (_2 . #location . only p)) . camelPositions

-- | The first location that counts as past the finish line.
finishLine :: Int
finishLine = undefined

-- -- | Slow shuffle, that's good enough for n = 5 (our usecase) while being very
-- -- simple
-- slowShuffle :: [a] -> g -> ([a], g)
-- slowShuffle xs g = go len g xs
--   where
--     go 0 g [] = ([], g)
--     go n g (x : xs) =
--       let (xs', g') = go (n - 1) g xs
--           (i, g'') = uniformR (0, len - 1) g'
--        in (insertAt i x : xs', g'')
--     go _ _ _ = error "invariant that first param is length of list broken"
--     len = length xs

-- | Shuffle that doesn't shuffle
dumbShuffle :: [a] -> g -> ([a], g)
dumbShuffle xs g = (xs, g)

randomCamel :: RandomGen g => g -> (Camel, g)
randomCamel gen =
  first toEnum . (`uniformR` gen) $
    over both fromEnum (minBound :: Camel, maxBound)

-- | TODO use Stateful random interface; better than using state explicitly
-- probably
randomStartingBoard :: RandomGen g => g -> (Board, g)
randomStartingBoard = runState do
  let camels = [minBound .. maxBound :: Camel]
  randomCamels <- state $ dumbShuffle camels
  camelRolls <- for randomCamels \c -> do
    roll <- state $ uniformR (0, 2)
    pure (c, roll)
  let startingPositions = rollsToStartingPositions camelRolls
  pure $ parseBoard startingPositions
  where
    rollsToStartingPositions xs = go xs []
    go [] acc = over (mapped . _2 . #camelsAbove) reverse acc
    go ((c, p) : xs) acc =
      go xs $
        (c, CamelPos p []) : flip map acc \case
          (c', CamelPos p' above) | p == p' -> (c', CamelPos p' (c : above))
          cp -> cp
    parseBoard
      ( sortOn fst ->
          [ (White, white),
            (Orange, orange),
            (Blue, blue),
            (Yellow, yellow),
            (Green, green)
            ]
        ) = Board {white, orange, blue, yellow, green}
    parseBoard _ = error "impossible: not every camel had a starting position"

advanceCamel :: Camel -> Int -> Board -> Board
advanceCamel c n = execState do
  oldLocation <- use $ camelPos c . #location
  camelPos c . #location += n
  newLocation <- use $ camelPos c . #location
  camelsAbove <- use $ camelPos c . #camelsAbove
  for_ camelsAbove \c ->
    camelPos c . #location += n
  let movedStack = c : camelsAbove
  camelsAtOldLocation <- camelsAtLocation oldLocation <$> get
  for_ camelsAtOldLocation \c -> do
    let err =
          error $
            "impossible: " ++ show c ++ " didn't have correct camels above it"
    camelPos c . #camelsAbove %= fromMaybe err . stripSuffix movedStack
  camelsAtNewLocation <- camelsAtLocation newLocation <$> get
  for_ camelsAtNewLocation \c ->
    camelPos c . #camelsAbove <>= movedStack

main :: IO ()
main = undefined
