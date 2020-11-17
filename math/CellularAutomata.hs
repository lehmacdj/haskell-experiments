#!/usr/bin/env stack
{- stack script
  --resolver lts-16.6
  --package random
  --package containers
  --package comonad
  --package diagrams
  --package diagrams-lib
  --package diagrams-svg
  --package directory
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Control.Comonad
import Control.Monad (replicateM)
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))
import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (index, shift)
import GHC.Generics
import GHC.Stack
import System.Directory
import System.Random

-- | A list that can be treated as wrapping around in a circle.
-- NOTE: this would be much more efficient to implement in terms of a random
-- access data structure
newtype CircularList a = CircularList {backingList :: NonEmpty a}
  deriving (Show, Read, Eq, Ord, Generic, Functor)

-- | Apply a function n times to a given value
-- taken from: protolude-0.3.0
applyN :: Int -> (a -> a) -> a -> a
applyN n f = Foldable.foldr (.) id (List.replicate n f)

instance Comonad CircularList where
  extract (CircularList (x :| _)) = x
  duplicate xs = CircularList (NE.zipWith shift shifts (NE.repeat xs))
    where
      shifts = 0 :| [1 .. NE.length (backingList xs) - 1]

-- | shift obeys the following law in interaction with index to determine which
-- way is shifting positive vs negative:
-- index n (shift m xs) == index (n + m) xs
shift :: HasCallStack => Int -> CircularList a -> CircularList a
shift n (CircularList xs) = CircularList (applyN modifiedShift shiftRight1 xs)
  where
    modifiedShift = n `mod` NE.length xs
    shiftRight1 xs@(_ :| []) = xs
    shiftRight1 (x :| (x' : xs)) = x' :| xs ++ [x]

index :: HasCallStack => Int -> CircularList a -> a
index n (CircularList xs) = xs NE.!! modifiedIndex
  where
    modifiedIndex = n `mod` NE.length xs

data TwoColorState = Dead | Alive
  deriving (Eq, Ord, Generic)

instance Show TwoColorState where
  show Dead = " "
  show Alive = "#"

type Neighborhood a = (a, a, a)

neighborhood :: CircularList a -> Neighborhood a
neighborhood xs = (index (-1) xs, index 0 xs, index 1 xs)

rule30 :: Neighborhood TwoColorState -> TwoColorState
rule30 = \case
  (Dead, Dead, Dead) -> Dead
  (Dead, Dead, Alive) -> Alive
  (Dead, Alive, Dead) -> Alive
  (Dead, Alive, Alive) -> Alive
  (Alive, Dead, Dead) -> Alive
  (Alive, Dead, Alive) -> Dead
  (Alive, Alive, Dead) -> Dead
  (Alive, Alive, Alive) -> Dead

rule110 :: Neighborhood TwoColorState -> TwoColorState
rule110 = \case
  (Dead, Dead, Dead) -> Dead
  (Dead, Dead, Alive) -> Alive
  (Dead, Alive, Dead) -> Alive
  (Dead, Alive, Alive) -> Alive
  (Alive, Dead, Dead) -> Dead
  (Alive, Dead, Alive) -> Alive
  (Alive, Alive, Dead) -> Alive
  (Alive, Alive, Alive) -> Dead

rule90 :: Neighborhood TwoColorState -> TwoColorState
rule90 = \case
  (Dead, Dead, Dead) -> Alive
  (Dead, Dead, Alive) -> Dead
  (Dead, Alive, Dead) -> Alive
  (Dead, Alive, Alive) -> Dead
  (Alive, Dead, Dead) -> Dead
  (Alive, Dead, Alive) -> Alive
  (Alive, Alive, Dead) -> Dead
  (Alive, Alive, Alive) -> Alive

stepAutomata :: (Neighborhood a -> a) -> CircularList a -> CircularList a
stepAutomata rule = extend (rule . neighborhood)

newtype History a = History {unHistory :: [CircularList a]}
  deriving (Eq, Ord, Generic)

instance Show a => Show (History a) where
  show = unlines . fmap showLine . unHistory
    where
      showLine = concat . NE.toList . fmap show . backingList

generateHistory ::
  HasCallStack =>
  (Neighborhood a -> a) ->
  Int ->
  CircularList a ->
  History a
generateHistory rule n start
  | n < 0 = error "n must be >= 0"
  | otherwise = History $ take n $ iterate (stepAutomata rule) start

-- | Starting state where only the element extracted by the comonad instance
-- is alive and every other cell is dead. List has the specified length.
-- errors if n < 1 since the list must be non-empty
singleCellAlive :: HasCallStack => Int -> CircularList TwoColorState
singleCellAlive n
  | n < 1 = error "n must be > 0"
  | otherwise = CircularList (Alive :| replicate (n - 1) Dead)

randomStartingState :: HasCallStack => Int -> IO (CircularList TwoColorState)
randomStartingState n
  | n < 1 = error "n must be > 0"
  | otherwise = CircularList . NE.fromList <$> replicateM n randomTwoColorState
  where
    randomTwoColorState = do
      randomBool <- randomIO
      if randomBool
        then pure Alive
        else pure Dead

toSquare :: TwoColorState -> Diagram B
toSquare = \case
  Dead -> fillColor black (square 1)
  Alive -> lineWidth none $ fillColor white $ square 1

renderRow :: CircularList TwoColorState -> Diagram B
renderRow xs = vcat $ map toSquare $ NE.toList (backingList xs)

renderHistory :: History TwoColorState -> Diagram B
renderHistory (History h) = hcat $ map renderRow h

ensureDirExists :: IO ()
ensureDirExists = createDirectoryIfMissing True "diagrams"

rendered :: FilePath -> Diagram B -> IO ()
rendered f d = ensureDirExists >> renderSVG filename (mkWidth 1000) d
  where
    filename = "diagrams/" ++ f ++ ".svg"

rendered' :: FilePath -> History TwoColorState -> IO ()
rendered' fp h = rendered fp (renderHistory h)

draw :: Diagram B -> IO ()
draw = rendered "tmp"

main :: IO ()
main = do
  randomState <- randomStartingState 100
  -- rendered' "rule90-random" $ generateHistory rule90 100 randomState
  -- rendered' "rule30-random" $ generateHistory rule30 100 randomState
  -- rendered' "rule110-random" $ generateHistory rule110 100 randomState
  rendered' "rule90-simple" $ generateHistory rule90 500 $ singleCellAlive 500
