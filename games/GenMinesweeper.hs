#!/usr/bin/env stack
{- stack
  script
  --resolver lts-16.6
  --package random
  --package containers
-}

-- | Generate a minesweeper board that renders as a nice playable game in
-- discord flavored markup.
module Main (main) where

import Control.Monad (guard)
import Data.Char (intToDigit)
import Data.Monoid
import Data.Set (Set, member, singleton)
import GHC.Stack
import System.Random

data Tile
  = Number Int
  | Mine
  deriving (Show, Eq, Ord)

-- | the height and width of the generated board
height, width :: Int
height = 10
width = 10

-- | check if width/height is in bounds
withinHeight, withinWidth :: Int -> Bool
withinHeight = (&&) <$> (>= 0) <*> (< height)
withinWidth = (&&) <$> (>= 0) <*> (< width)

-- | width/height ranges
heightRange, widthRange :: (Int, Int)
heightRange = (0, height - 1)
widthRange = (0, width - 1)

-- | the number of mines to generate
mineCount :: Int
mineCount = 12

-- | choose a new unique (relative to the provided set) location for a mine
-- note: theoretically could not terminate if we get really unlucky
newMine :: Set (Int, Int) -> IO (Int, Int)
newMine mines = do
  x <- randomRIO widthRange
  y <- randomRIO heightRange
  if not ((x, y) `member` mines)
    then pure (x, y)
    else newMine mines

generateMines :: HasCallStack => IO (Set (Int, Int))
generateMines = go mineCount mempty
  where
    go 0 xs = pure xs
    go n xs
      | n > 0 = do
        mine <- newMine xs
        go (n - 1) (xs <> singleton mine)
      | otherwise = error "invalid mine count: can't have mines < 0"

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = do
  dx <- [-1 .. 1]
  dy <- [-1 .. 1]
  guard $ dx /= 0 || dy /= 0
  pure (x + dx, y + dy)

-- | Count of mines adjacent to a location
adjacentMines :: Set (Int, Int) -> (Int, Int) -> Int
adjacentMines mines = length . filter (`member` mines) . neighbors

tileAtLocation :: Set (Int, Int) -> (Int, Int) -> Tile
tileAtLocation mines point
  | point `member` mines = Mine
  | otherwise = Number (adjacentMines mines point)

-- | Generate the correct tiles given mine placemets
minesToBoard :: Set (Int, Int) -> [[Tile]]
minesToBoard mines =
  [ [tileAtLocation mines (x, y) | x <- [0 .. width - 1]]
    | y <- [0 .. height - 1]
  ]

-- | a couple of unicode characters we need
variationSelector16, combiningEnclosingKeycap :: Char
variationSelector16 = '\xFE0F'
combiningEnclosingKeycap = '\x20E3'

-- | displays a keycapped emoji such as 3️⃣  corresponding to the given digit
digitToKeycap :: HasCallStack => Int -> String
digitToKeycap x
  | x >= 0 && x <= 9 =
    [intToDigit x, variationSelector16, combiningEnclosingKeycap]
  | otherwise = error "out of bounds digit"

-- | turn a tile into the corresponding emoji
toEmoji :: Tile -> String
toEmoji Mine = "💥"
toEmoji (Number n)
  | n >= 0 && n <= 8 = digitToKeycap n
  | otherwise = error "out of bounds number of neighbors for minesweeper"

spoiler :: String -> String
spoiler x = "||" ++ x ++ "||"

renderRow :: [Tile] -> String
renderRow = concatMap (spoiler . toEmoji)

-- | render spoiler tags around each tile in the grid + turn Tiles into emoji
renderGrid :: [[Tile]] -> String
renderGrid = unlines . map renderRow

main :: IO ()
main = do
  mines <- generateMines
  putStr $ renderGrid (minesToBoard mines)
