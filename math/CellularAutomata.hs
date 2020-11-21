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
  --package array
  --package JuicyPixels
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Codec.Picture
import Control.Comonad
import Control.Monad (replicateM)
import Data.Array (Array, (!))
import qualified Data.Array as Array
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))
import Debug.Trace
import qualified Diagrams.Backend.SVG as Diagrams
import Diagrams.Backend.SVG (B)
import qualified Diagrams.Prelude as Diagrams
import Diagrams.Prelude (Diagram)
import GHC.Generics
import GHC.Stack
import System.Directory
import System.Random

-- | A list that can be treated as wrapping around in a circle.
-- NOTE: this would be much more efficient to implement in terms of a random
-- access data structure
newtype CircularList a = CircularList {backingArray :: Array Int a}
  deriving (Show, Read, Eq, Ord, Generic, Functor)

linearize :: CircularList a -> [a]
linearize = Array.elems . backingArray

arraySize :: Array Int a -> Int
arraySize arr = case Array.bounds arr of
  (low, high) -> high - low + 1

instance Comonad CircularList where
  extract (CircularList arr) = arr ! fst (Array.bounds arr)
  duplicate (CircularList arr) =
    CircularList . Array.listArray (Array.bounds arr) $
      [shift n (CircularList arr) | n <- [0 .. arraySize arr - 1]]

traceMsgShowId :: Show a => String -> a -> a
traceMsgShowId msg a = trace (msg ++ show a) a

-- | shift obeys the following law in interaction with index to determine which
-- way is shifting positive vs negative:
-- index n (shift m xs) == index (n + m) xs
shift :: HasCallStack => Int -> CircularList a -> CircularList a
shift n (CircularList arr) =
  CircularList $
    Array.ixmap (Array.bounds arr) remapIndex arr
  where
    modifiedShift = n `mod` arraySize arr
    remapIndex i = (i + modifiedShift) `mod` arraySize arr

index :: HasCallStack => Int -> CircularList a -> a
index n (CircularList arr) = arr ! modifiedIndex
  where
    modifiedIndex = n `mod` arraySize arr

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
      showLine = concatMap show . linearize

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
  | otherwise =
    CircularList $
      Array.listArray (0, n - 1) (Alive : replicate (n - 1) Dead)

randomStartingState :: HasCallStack => Int -> IO (CircularList TwoColorState)
randomStartingState n
  | n < 1 = error "n must be > 0"
  | otherwise =
    CircularList . Array.listArray (0, n - 1)
      <$> replicateM n randomTwoColorState
  where
    randomTwoColorState = do
      randomBool <- randomIO
      if randomBool
        then pure Alive
        else pure Dead

diagramOfCell :: TwoColorState -> Diagram B
diagramOfCell = \case
  Dead ->
    Diagrams.lineColor Diagrams.black $
      Diagrams.fillColor Diagrams.black $
        Diagrams.square 1
  Alive ->
    Diagrams.lineColor Diagrams.white $
      Diagrams.fillColor Diagrams.white $
        Diagrams.square 1

diagramOfRow :: CircularList TwoColorState -> Diagram B
diagramOfRow xs = Diagrams.hcat $ map diagramOfCell $ linearize xs

diagramOfHistory :: History TwoColorState -> Diagram B
diagramOfHistory (History h) = Diagrams.vcat $ map diagramOfRow h

ensureDirExists :: IO ()
ensureDirExists = createDirectoryIfMissing True "images"

pathOfName :: String -> String -> FilePath
pathOfName name extension = "images/" ++ name ++ "." ++ extension

renderDiagramSvg :: FilePath -> Diagram B -> IO ()
renderDiagramSvg name d = do
  ensureDirExists
  Diagrams.renderSVG (pathOfName name "svg") (Diagrams.mkWidth 1000) d

renderSvg :: FilePath -> History TwoColorState -> IO ()
renderSvg name h = renderDiagramSvg name (diagramOfHistory h)

pixelOfCell :: TwoColorState -> Pixel8
pixelOfCell = \case
  Dead -> minBound
  Alive -> maxBound

imageOfHistory :: HasCallStack => History TwoColorState -> Image Pixel8
imageOfHistory (History rows) = generateImage pixelColor width height
  where
    pixelColor x y = pixelOfCell $ backingArray (rows !! y) ! x
    height = length rows
    width
      | height == 0 = error "no rows specified, can't determine width"
      | otherwise = arraySize . backingArray . head $ rows

renderPng :: FilePath -> History TwoColorState -> IO ()
renderPng name h = do
  ensureDirExists
  writePng (pathOfName name "png") (imageOfHistory h)

main :: IO ()
main = do
  startingState <- randomStartingState 1000
  renderPng "rule110-1000x1000" $ generateHistory rule110 1000 startingState
