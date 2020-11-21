#!/usr/bin/env stack
{- stack ghci
    --resolver lts-16.11
    --package typed-process
    --package bytestring
    --package containers
    --package combinatorial
    --package temporary
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

-- | Requires geng and directg to be built in "nauty27r1" which is the
-- version of nauty that I acquired.
-- nautyPath must be set to a value that is valid for your system for this to
-- work properly.
module Main (main) where

import Combinatorics
import Control.Monad (join, (>=>))
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Foldable (for_)
import Data.Graph
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Debug.Trace
import GHC.Stack (HasCallStack)
import System.Environment (getExecutablePath)
import System.Exit
import System.IO
import System.IO.Temp
import System.Process.Typed
import Text.Read (readMaybe)

type LByteString = LBS.ByteString

-- * parameters that one may want to change

nautyPath :: FilePath
nautyPath = "/Users/devin/src/haskell/experiments/math/graphs/nauty27r1/"

nodeCount :: Int
nodeCount = 7

-- * program code

-- | note: this does override all other entries in path other than what is
-- required to use nauty executables
withNautyInPath :: ProcessConfig i o e -> ProcessConfig i o e
withNautyInPath = setEnv [("PATH", nautyPath)]

-- | Generates non-isomorphic directed graphs
-- stdout of this (to be read with 'readProcess_') is the simple output,
-- #n #e e0l e0r ... enl enr
graphGenerator ::
  -- | the number of nodes
  Int ->
  -- | range (min, max) of number of edges in the graphs
  (Int, Int) ->
  ProcessConfig () () ()
graphGenerator n (eMin, eMax) =
  withNautyInPath $
    shell $ "geng " ++ show n ++ " " ++ edgeRange ++ " | directg -T"
  where
    edgeRange = show eMin ++ ":" ++ show eMax

-- | The maximum number of edges in a graph
maxEdges :: Int -> Int
maxEdges n = n * (n - 1)

-- | range of all numbers of edges for a given number of nodes
allEdges :: Int -> (Int, Int)
allEdges n = (0, maxEdges n)

onlyEdgeLength :: Int -> (Int, Int)
onlyEdgeLength x = (x, x)

-- | group elements by twos. each element occurs in exactly one tuple
--
-- Examples:
-- paired [1, 2, 3, 4, 5] == Nothing
-- paired [1, 2, 3, 4] == Just [(1, 2), (3, 4)]
-- paired [1, 2] == Just [(1, 2)]
-- paired [1] == Nothing
-- paired [] == Just []
paired :: [a] -> Maybe [(a, a)]
paired = \case
  [] -> Just []
  x : y : ys -> ((x, y) :) <$> paired ys
  [x] -> Nothing

parseEdges :: [String] -> Maybe [Edge]
parseEdges = traverse readMaybe >=> paired

-- | Reads a graph from a graph specified in the format:
-- #n #e e0l e0r ... enl enr
-- errors on invalid format
parseGraph :: HasCallStack => String -> Graph
parseGraph graphSpec = case words graphSpec of
  (readMaybe -> Just nc) : (readMaybe -> Just ec) : (parseEdges -> Just edges)
    | nc >= 0 && ec >= 0 && ec <= maxEdges nc && length edges == ec ->
      buildG (0, nc) edges
  _ ->
    error
      "invalid format: format must be \"#n #e e0l e0r ... enl enr\" \
      \where #n, #e in N, #e <= #n * (#n - 1), \
      \and the number of edges is the edge count"

-- | get every pair of adjacent elements, order is respected
--
-- Examples:
-- pairwise [1, 2, 3] == [(1, 2), (2, 3)]
-- pairwise [1] == []
-- pairwise [] == []
pairwise :: [a] -> [(a, a)]
pairwise = \case
  [] -> []
  [_] -> []
  x : y : ys -> (x, y) : pairwise (y : ys)

-- | polynomial time for any fixed n, exponential in n. brute force algorthm,
-- just makes every possible path
containsPathOfLength :: Int -> Graph -> Bool
containsPathOfLength n g = any (all hasEdge . pairwise) paths
  where
    es = Set.fromList $ edges g
    hasEdge e = e `Set.member` es
    paths = variate (n + 1) (vertices g)

-- | true if containsPathOfLength passes the tests
test_containsPathOfLength :: Bool
test_containsPathOfLength =
  and (runTests positiveTests) && not (or (runTests negativeTests))
  where
    runTests = map (uncurry containsPathOfLength)
    positiveTests =
      [ (2, buildG (0, 3) [(0, 1), (1, 2)]),
        (3, buildG (0, 3) (join $ mapMaybe paired $ variate 2 [0 .. 3]))
      ]
    negativeTests =
      [ (3, buildG (0, 4) [])
      ]

doesEdgeCountHaveP5 :: Int -> IO (Either () [Graph])
doesEdgeCountHaveP5 m = do
  putStrLn $ "starting edge count: " ++ show m
  (LBS.unpack -> out, _) <-
    readProcess_ $
      graphGenerator nodeCount (onlyEdgeLength m)
  let graphs = parseGraph <$> lines out
  if all (containsPathOfLength 4) graphs
    then pure $ Left ()
    else pure $ Right graphs

prettyPrintGraphs :: [Graph] -> String
prettyPrintGraphs = unlines . fmap show

main :: IO ()
main =
  for_ (reverse (uncurry enumFromTo (allEdges nodeCount))) $ \m -> do
    answer <- doesEdgeCountHaveP5 m
    case answer of
      Left () -> pure ()
      Right graphs -> do
        putStrLn $ "succeeded with n=" ++ show nodeCount ++ ", m=" ++ show m
        exitSuccess
