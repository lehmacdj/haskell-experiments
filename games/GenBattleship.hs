#!/usr/bin/env stack
{- stack
  script
  --resolver lts-18.18
  --package random
  --package containers
-}

{-# LANGUAGE LambdaCase #-}

-- | Generate a minesweeper board that renders as a nice playable game in
-- discord flavored markup.
module Main (main) where

import Control.Monad (guard, replicateM_)
import Data.Char (intToDigit)
import Data.Foldable (for_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack
import System.Random

type Emoji = String

data Tile
  = Water
  | Ship Emoji
  deriving (Show, Eq, Ord)

data Direction = N | E | S | W
  deriving (Show, Enum, Bounded)

instance Random Direction where
  random g =
    let directions = [minBound .. maxBound]
        (ix, g') = randomR (0, length directions - 1) g
     in (directions !! ix, g')
  randomR (minD, maxD) g =
    let directions = [minD .. maxD]
        (ix, g') = randomR (0, length directions - 1) g
     in (directions !! ix, g')

data ShipLoc = ShipLoc
  { symbol :: Emoji,
    coords :: (Int, Int),
    direction :: Direction,
    size :: Int
  }
  deriving (Show)

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

vecFromDir :: Direction -> (Int, Int)
vecFromDir = \case
  N -> (0, 1)
  S -> (0, -1)
  E -> (1, 0)
  W -> (-1, 0)

scalarMult :: Int -> (Int, Int) -> (Int, Int)
scalarMult s (x, y) = (s * x, s * y)

vecSum :: (Int, Int) -> (Int, Int) -> (Int, Int)
vecSum (x, y) (x', y') = (x + x', y + y')

-- | The spaces that a ship takes up on the board
spaces :: ShipLoc -> Set (Int, Int)
spaces sl@(ShipLoc _ (x, y) d s) =
  Set.fromList $
    vecSum
      <$> [(x, y)]
      <*> (scalarMult <$> [0 .. s - 1] <*> [vecFromDir d])

collides :: ShipLoc -> ShipLoc -> Bool
collides s1 s2 = not . null $ spaces s1 `Set.intersection` spaces s2

inbounds :: ShipLoc -> Bool
inbounds s = all (\(x, y) -> withinWidth x && withinHeight y) $ spaces s

none :: Foldable f => (a -> Bool) -> f a -> Bool
none p = not . any p

-- TODO: it appears as though this is still generating overlapping ships
newShip :: (Emoji, Int) -> [ShipLoc] -> IO ShipLoc
newShip (symb, size) ships = do
  x <- randomRIO widthRange
  y <- randomRIO heightRange
  d <- randomIO
  let ship = ShipLoc symb (x, y) d size
  if not $ inbounds ship
    then newShip (symb, size) ships
    else
      if none (collides ship) ships
        then pure ship
        else newShip (symb, size) ships

-- | Generate non intersecting ship locations randomly. Theoretically doesn't
-- terminate if we get really unlucky.
placeShips ::
  -- | the ships to place on the board.
  -- The char specifies the character to display for the ship.
  -- The int specifies the length of the ship.
  [(Emoji, Int)] ->
  IO [ShipLoc]
placeShips = go []
  where
    go ships = \case
      [] -> pure ships
      (symb, size) : remainingShipDescs -> do
        ship <- newShip (symb, size) ships
        go (ship : ships) remainingShipDescs

-- | take a ship and turn it into a map converting
explodeShip :: ShipLoc -> Map (Int, Int) Emoji
explodeShip s = Map.fromSet (const (symbol s)) (spaces s)

tileAtLocation :: Map (Int, Int) Emoji -> (Int, Int) -> Tile
tileAtLocation shipSymbols point =
  maybe Water Ship $ Map.lookup point shipSymbols

shipsToBoard :: Map (Int, Int) Emoji -> [[Tile]]
shipsToBoard shipSymbols =
  [ [tileAtLocation shipSymbols (x, y) | x <- [0 .. width - 1]]
    | y <- [0 .. height - 1]
  ]

-- | turn a tile into the corresponding emoji
toEmoji :: Tile -> Emoji
toEmoji Water = "🌊"
toEmoji (Ship symb) = symb

spoiler :: String -> String
spoiler x = "||" ++ x ++ "||"

renderRow :: [Tile] -> String
renderRow = concatMap (spoiler . toEmoji)

-- | render spoiler tags around each tile in the grid + turn Tiles into emoji
renderGrid :: [[Tile]] -> String
renderGrid = unlines . map renderRow

main :: IO ()
main = do
  ships <- placeShips [("💥", 2), ("☀️", 3), ("🔥", 4), ("⚡", 5)]
  let shipSymbols = mconcat $ map explodeShip ships
  putStr $ renderGrid (shipsToBoard shipSymbols)
