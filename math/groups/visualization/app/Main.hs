{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import ClassyPrelude
import Codec.Picture
import Codec.Picture.Types (Pixel16 (..))
import Control.Lens (itraverse)
import Control.Monad.Logic.Class
import Data.Group
import Data.Monoid (Sum (..))
import Data.Proxy (Proxy (Proxy))
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Time (diffUTCTime)
import qualified Data.Vector as Vector
import Data.Word (Word16)
import GHC.Stack (HasCallStack)
import GHC.TypeLits (KnownNat, Nat (..), SomeNat (..), natVal, someNatVal)
import System.Directory
import Prelude (showParen, showString)

intValue :: forall n. KnownNat n => Int
intValue = fromInteger (natVal (Proxy @n))

mtimes :: (Monoid m, Integral b) => b -> m -> m
mtimes k a
  | k == 0 = mempty
  | otherwise = stimes k a

gtimes :: (Group g, Integral b) => b -> g -> g
gtimes k a
  | k < 0 = stimes (-k) (invert a)
  | otherwise = mtimes k a

-- | Represents the symmetric group on n elements. The Vector must be a
-- permuation on n elements. i.e. a list of length n with elements precisely
-- [0 .. n - 1]
newtype Sym (n :: Nat) = UnsafePermutation {getPermutation :: Vector Int}
  deriving (Eq, Ord)

instance Show (Sym n) where
  showsPrec p xs =
    showParen (p > 10) $
      showString "perm " . showsPrec 11 (toList (getPermutation xs))

-- | A representation of a number in the Factorial Number System where
-- the first element of the list is the most significant digit and subsequent
-- digits are less significant
type FactorialDigitRep n = [n]

toFactorialNumberSystem :: (Integral n, Num n) => n -> FactorialDigitRep n
toFactorialNumberSystem = go 1 []
  where
    go !d rep !n =
      let (!q, !r) = n `divMod` d
       in if q == 0
            then r : rep
            else go (d + 1) (r : rep) q

fromFactorialNumberSystem :: Num n => FactorialDigitRep n -> n
fromFactorialNumberSystem = go 1 1 0 . reverse
  where
    go _ _ !res [] = res
    go !kn !fact !res (d : ds) =
      go (kn + 1) (kn * fact) (res + fact * d) ds

padToLength :: Num n => Int -> FactorialDigitRep n -> FactorialDigitRep n
padToLength len xs = replicate amountToPad 0 ++ xs
  where
    amountToPad = max 0 (len - length xs)

-- | Compute a Lehmer code from a permutation of [0..n-1]
lehmerEncode :: [Int] -> [Int]
lehmerEncode = \case
  [] -> []
  (x : xs) -> x : lehmerEncode (map (predIfGt x) xs)
  where
    predIfGt !k !n
      | n > k = n - 1
      | otherwise = n

-- | Compute a permutation of [0..n-1] from a Lehmer code
lehmerDecode :: [Int] -> [Int]
lehmerDecode = foldr (\x acc -> x : map (succIfGe x) acc) []
  where
    succIfGe !k !n
      | n >= k = n + 1
      | otherwise = n

-- | Given by the total ordering derived from the lexicographic order.
-- Uses procedure roughly outlined by
-- https://en.wikipedia.org/wiki/Permutation#Numbering_permutations
-- e.g.
-- fromEnum (perm [0, 1, 2]) = 0
-- fromEnum (perm [0, 2, 1]) = 1
-- fromEnum (perm [1, 0, 2]) = 2
-- fromEnum (perm [1, 2, 0]) = 3
-- fromEnum (perm [2, 0, 1]) = 4
-- fromEnum (perm [2, 1, 0]) = 5
instance forall n. KnownNat n => Enum (Sym n) where
  toEnum =
    permEx
      . lehmerDecode
      . padToLength (intValue @n)
      . toFactorialNumberSystem
  fromEnum = fromFactorialNumberSystem . lehmerEncode . toList . getPermutation

-- | Given by the total ordering derived from the lexicographic order.
instance KnownNat n => Bounded (Sym n) where
  minBound = mempty
  maxBound =
    let largestIx = intValue @n - 1
     in permEx [largestIx, largestIx - 1 .. 0]

-- | Not safe, doesn't check uniqueness of elements
perm :: forall n. KnownNat n => [Int] -> Maybe (Sym n)
perm xs
  | len == n && all ((&&) <$> (< n) <*> (>= 0)) xs = Just $ UnsafePermutation v
  | otherwise = Nothing
  where
    v = Vector.fromList xs
    len = length v
    n = intValue @n

permEx :: (KnownNat n, HasCallStack) => [Int] -> Sym n
permEx as = fromMaybe ex . perm $ as
  where
    ex = error $ "permutation " ++ show as ++ " doesn't match invariant"

getPerm :: Sym n -> [Int]
getPerm = toList . getPermutation

instance Semigroup (Sym n) where
  xs <> ys = UnsafePermutation $ fmap (indexEx (getPermutation xs)) (getPermutation ys)

instance KnownNat n => Monoid (Sym n) where
  mempty = permEx [0 .. intValue @n - 1]

instance KnownNat n => Group (Sym n) where
  invert =
    UnsafePermutation
      . fmap fst
      . sortOn snd
      . zip (getPermutation (mempty :: Sym n))
      . getPermutation

symmetricGroupElements :: forall n. KnownNat n => [Sym n]
symmetricGroupElements =
  permEx <$> permutations [0 .. intValue @n - 1]

-- | Representation of D_2n in S_n
dihedralGroupElements :: forall n. KnownNat n => [Sym n]
dihedralGroupElements =
  cyclicSubgroup ++ map (<> s) cyclicSubgroup
  where
    cyclicSubgroup = [gtimes i r | i <- [0 .. intValue @n - 1]]
    r = permEx $ [1 .. intValue @n - 1] ++ [0]
    s = permEx . reverse $ [0 .. intValue @n - 1]

data Z2 = Zero | One
  deriving (Show, Eq, Ord)

instance Semigroup Z2 where
  Zero <> x = x
  x <> Zero = x
  One <> One = Zero

instance Monoid Z2 where
  mempty = Zero

instance Group Z2 where
  invert x = x

type Z = Sum Integer

data Symbol a = Normal a | Inverted a
  deriving (Show, Eq, Ord)

isNormal, isInverted :: Symbol a -> Bool
isNormal = \case
  Normal _ -> True
  Inverted _ -> False
isInverted = not . isNormal

reduces :: Eq a => Symbol a -> Symbol a -> Bool
reduces (Normal x) (Inverted x') | x == x' = True
reduces (Inverted x) (Normal x') | x == x' = True
reduces _ _ = False

newtype FG a = Word {getSymbols :: Seq (Symbol a)}
  deriving (Show, Eq)

instance Ord a => Ord (FG a) where
  compare = compare `on` (\(Word xs) -> (length xs, xs))

instance Eq a => Semigroup (FG a) where
  Word xs'@(xs :|> x) <> Word ys'@(y :<| ys) = case (x, y) of
    (Normal _, Normal _) -> Word (xs' <> ys')
    (Inverted _, Inverted _) -> Word (xs' <> ys')
    (Normal x, Inverted y)
      | x == y -> Word xs <> Word ys
      | otherwise -> Word (xs' <> ys')
    (Inverted x, Normal y)
      | x == y -> Word xs <> Word ys
      | otherwise -> Word (xs' <> ys')
  Word xs <> Word ys = Word (xs <> ys)

-- | Given a list of symbls; generate all words that draw from it.
freeElements :: Eq a => [a] -> [FG a]
freeElements xs =
  Word mempty : do
    xs >>- \x ->
      [Normal x, Inverted x] >>- \symbol ->
        freeElements xs >>= \case
          Word Empty -> pure $ Word (pure symbol)
          Word ys@(_ :|> y) -> do
            guard (not (reduces symbol y))
            pure $ Word (ys :|> symbol)

instance Eq a => Monoid (FG a) where
  mempty = Word mempty

wordLength :: FG a -> Int
wordLength = length . getSymbols

invertSymbol :: Symbol a -> Symbol a
invertSymbol = \case
  Normal a -> Inverted a
  Inverted a -> Normal a

instance Eq a => Group (FG a) where
  invert (Word xs) = Word $ invertSymbol <$> reverse xs

mkImage ::
  forall m px.
  (Pixel px, Show m, Monoid m, Ord m) =>
  [m] ->
  (m -> px) ->
  Image px
mkImage (Vector.fromList -> es) toColor = generateImage pixelAt width height
  where
    width = length es
    height = width
    pixelAt x y = toColor $ (es `indexEx` x) <> (es `indexEx` y)

ensureDirExists :: IO ()
ensureDirExists = createDirectoryIfMissing True "images"

pathOfName :: String -> String -> FilePath
pathOfName name extension = "images/" ++ name ++ "." ++ extension

render ::
  forall m px.
  (Pixel px, PngSavable px, Show m, Monoid m, Ord m) =>
  FilePath ->
  [m] ->
  (m -> px) ->
  IO ()
render name es toColor = do
  ensureDirExists
  say $ "rendering: " ++ pack name
  startTime <- getCurrentTime
  let image = mkImage es toColor
  writePng (pathOfName name "png") image
  endTime <- getCurrentTime
  let duration = endTime `diffUTCTime` startTime
  say $ "took " ++ tshow duration ++ " while rendering " ++ pack name

black, white, red :: PixelRGB16
black = PixelRGB16 0 0 0
white = PixelRGB16 maxBound maxBound maxBound
red = PixelRGB16 maxBound 0 0

-- | Color based on a metric. The metric should return a value between 0 and
-- max
colorViaMetric :: (Int -> Int -> px) -> (a -> Int) -> Int -> a -> px
colorViaMetric toPixel toInt m v = toPixel (toInt v) m

greyscaleViaMetric :: (a -> Int) -> Int -> a -> PixelRGB16
greyscaleViaMetric = colorViaMetric greySpectrum

colorViaBoundedEnum :: forall a px. (Enum a, Bounded a) => (Int -> Int -> px) -> a -> px
colorViaBoundedEnum toPixel = colorViaMetric toPixel toInt (toInt (maxBound @a))
  where
    toInt x = fromEnum x - fromEnum (minBound @a)

greyscaleViaBoundedEnum :: (Enum a, Bounded a) => a -> PixelRGB16
greyscaleViaBoundedEnum = colorViaBoundedEnum greySpectrum

-- | Produce a grey color based on the position between 0 and a maximum value.
-- If the provided value is out of bounds, the color is red.
greySpectrum :: Int -> Int -> PixelRGB16
greySpectrum i m =
  let frac = fromIntegral i / fromIntegral m
      full = fromIntegral (maxBound :: Word16) :: Double
      v = round (frac * full)
      result
        | frac > 1 || frac < 0 = red
        | otherwise = PixelRGB16 v v v
   in result

gradedColoring :: Int -> FG a -> PixelRGB16
gradedColoring = greyscaleViaMetric wordLength

renderZ :: Integer -> IO ()
renderZ n =
  render @Z
    ("First" ++ show n ++ "-Z")
    (Sum <$> [0 .. n - 1])
    (greyscaleViaMetric (fromInteger . getSum) (fromInteger (2 * n)))

renderSymLexicographic :: Integer -> IO ()
renderSymLexicographic n = case someNatVal n of
  Just (SomeNat (_ :: Proxy n)) ->
    render @(Sym n)
      ("Sym-" ++ show n ++ "-Lexicographic")
      (sort symmetricGroupElements)
      greyscaleViaBoundedEnum
  Nothing -> error "invalid negative number provided as size of symmetric group"

renderDihedral :: Integer -> IO ()
renderDihedral n
  | n `mod` 2 == 1 = error "the dihedral group is only defined for even n"
  | otherwise =
    let m = n `div` 2
     in case someNatVal m of
          Just (SomeNat (_ :: Proxy m)) ->
            render @(Sym m)
              ("Dihedral-" ++ show n)
              (sort dihedralGroupElements)
              greyscaleViaBoundedEnum
          Nothing -> error "invalid negative number provided as size of symmetric group"

renderFree :: (Show a, Ord a) => [a] -> Int -> IO ()
renderFree generators n =
  render
    ("FreeGroupOn-" ++ show generators ++ "-First-" ++ show n)
    es
    (gradedColoring (2 * maxLenEs))
  where
    freeGroup = freeElements generators
    es = sort $ take n freeGroup
    maxLenEs = maximum (1 `ncons` fmap wordLength es)

main :: IO ()
main = do
  renderDihedral 500
  say "Done!"
