{-# LANGUAGE DeriveFunctor #-}

import Data.Functor.Foldable
import Numeric.Natural
import Data.Function (on)
import Data.List (sortBy, maximumBy)
import Test.QuickCheck

-- memoization toolkit
data TreeF a r = LeafF
               | NodeF r a r
               deriving (Functor, Show)

type Tree a = Fix (TreeF a)

leaf :: Tree a
leaf = Fix LeafF

node :: Tree a -> a -> Tree a -> Tree a
node l x r = Fix (NodeF l x r)

natTree :: Tree Natural
natTree = ana coalg (0, 1) where
  coalg (n, s) = NodeF (n + s, s') n (n + 2 * s, s')
    where s' = 2 * s

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = cata alg where
  -- infinite data structure means this will loop forever?
  alg LeafF = leaf
  alg (NodeF l x r) = node l (f x) r

index :: Natural -> Tree a -> a
index _ (Fix LeafF) = error "out of bounds"
index n (Fix (NodeF l x r))
  | n == 0 = x
  | n `mod` 2 == 1 = index (n `div` 2) l
  | otherwise = index (n `div` 2 - 1) r

toList :: Tree a -> [a]
toList t = map (`index` t) [0..]

data Interval = Interval
  { start :: Int
  , end :: Int
  , weight :: Int
  }
  deriving (Show)

-- return the subset of input intervals with maximum weight possible
schedule :: [Interval] -> [Interval]
schedule intervals = fastSchedule (fromIntegral $ length intervals)
  where sortedIntervals = sortBy (compare `on` end) intervals
        -- the ith job indexing from 1
        job = (sortedIntervals !!) . subtract 1
        -- compute the next job to use when accepting a time interval
        p 0 = 0
        p n = length $ takeWhile ((<=si) . end) sortedIntervals where
          si = start (job n)
        -- use open recursion so that we can memoize this function
        schedule' _ 0 = []
        schedule' mf n = maximumBy (compare `on` (sum . map weight)) [notUsing, using] where
          notUsing = mf $ n - 1
          using = job n : mf (p n)
        -- memoization table and function
        fastSchedule = (`index` scheduleTree) . fromIntegral
        scheduleTree = mapTree (schedule' fastSchedule . fromIntegral) natTree

scenario :: [Interval]
scenario =
  [ Interval 0 3 8
  , Interval 2 5 3
  , Interval 4 7 12
  , Interval 6 9 9
  , Interval 8 15 7
  , Interval 11 12 10
  , Interval 14 16 5
  ]

prop_natTree :: Natural -> Bool
prop_natTree n = n `index` natTree == n

main :: IO ()
main = do
  quickCheck prop_natTree
  print $ show $ schedule scenario
