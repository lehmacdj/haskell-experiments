{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
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
import qualified Data.Vector as Vector
import Data.Word (Word16)
import GHC.Stack (HasCallStack)
import GHC.TypeLits (KnownNat, Nat (..), natVal)
import System.Directory
import Prelude (showParen, showString)

-- | Represents the symmetric group on n elements. The Vector must be a
-- permuation on n elements. i.e. a list of length n with elements precisely
-- [0 .. n - 1]
newtype Sym (n :: Nat) = UnsafePermutation {getPermutation :: Vector Int}
  deriving (Eq, Ord)

instance Show (Sym n) where
  showsPrec p xs =
    showParen (p > 10) $
      showString "perm " . showsPrec 11 (toList (getPermutation xs))

perm :: forall n. KnownNat n => [Int] -> Maybe (Sym n)
perm xs
  | len == n && all ((&&) <$> (< n) <*> (>= 0)) xs = Just $ UnsafePermutation v
  | otherwise = Nothing
  where
    v = Vector.fromList xs
    len = length v
    n = fromInteger (natVal (Proxy @n))

permEx :: (KnownNat n, HasCallStack) => [Int] -> Sym n
permEx = fromMaybe ex . perm
  where
    ex = error "permutation doesn't match invariant"

getPerm :: Sym n -> [Int]
getPerm = toList . getPermutation

instance Semigroup (Sym n) where
  xs <> ys = UnsafePermutation $ fmap (indexEx (getPermutation xs)) (getPermutation ys)

instance KnownNat n => Monoid (Sym n) where
  mempty = permEx [0 .. fromInteger (natVal (Proxy @n)) - 1]

instance KnownNat n => Group (Sym n) where
  invert =
    UnsafePermutation
      . fmap fst
      . sortOn snd
      . zip (getPermutation (mempty :: Sym n))
      . getPermutation

symmetricGroupElements :: forall n. KnownNat n => [Sym n]
symmetricGroupElements =
  permEx <$> permutations [0 .. fromInteger (natVal (Proxy @n)) - 1]

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

-- | Given a list of symbols; generate all words that draw from it.
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
mkImage es toColor = generateImage pixelAt width height
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
  let image = mkImage es toColor
  writePng (pathOfName name "png") image

black, white, red :: PixelRGB16
black = PixelRGB16 0 0 0
white = PixelRGB16 maxBound maxBound maxBound
red = PixelRGB16 maxBound 0 0

greyValue :: Int -> Int -> PixelRGB16
greyValue i m =
  let frac = fromIntegral i / fromIntegral m
      full = fromIntegral (maxBound :: Word16) :: Double
      v = round (frac * full)
      result
        | frac > 1 = red
        | otherwise = PixelRGB16 v v v
   in result

greyscaled :: forall a. Ord a => [a] -> a -> PixelRGB16
greyscaled xs x = fromMaybe red $ lookup x cmap
  where
    len
      | null xs = 1
      | otherwise = length xs - 1
    cmap :: Map a PixelRGB16
    cmap =
      mapFromList . runIdentity $
        itraverse (\i x -> Identity (x, greyValue i len)) xs

gradedColoring :: Int -> FG a -> PixelRGB16
gradedColoring maxLength x = greyValue (wordLength x) maxLength

renderZ :: Integer -> IO ()
renderZ n =
  render @Z
    ("First" ++ show n ++ "-Z")
    (Sum <$> [0 .. n - 1])
    (greyscaled (Sum <$> [0 .. 2 * n]))

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
  renderFree [True, False] 20
  say "Done!"
