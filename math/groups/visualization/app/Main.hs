{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Word (Word16)
import GHC.Stack (HasCallStack)
import System.Directory
import Prelude (showParen, showString)

perm :: HasCallStack => [Int] -> Sn
perm xs
  | not isValid = error "invalid indices specified for permutation"
  | otherwise = Sigma (\i -> unwrap $ lookup i m) (Just len)
  where
    isValid = all (\x -> x < len && x >= 0) xs && ordNub xs == xs
    unwrap :: HasCallStack => Maybe a -> a
    unwrap = fromMaybe (error "invalid index for permutation")
    m :: IntMap Int
    m = mapFromList $ [0 .. l] `zip` xs
    l = len - 1
    len = length xs

data Sn = Sigma
  { sigma :: Int -> Int,
    n :: Maybe Int
  }

getPerm :: Sn -> [Int]
getPerm Sigma {..} =
  let r = sigma <$> [0 .. fromMaybe 0 n - 1]
   in if isJust $
        foldl'
          ( \a x -> case a of
              Just x' | x == x' -> Just $ x + 1
              _ -> Nothing
          )
          (Just 0)
          r
        then []
        else r

instance Show Sn where
  showsPrec p = \case
    Sigma {n = Nothing, ..} -> showString "identity"
    e@Sigma {n = Just n, ..}
      | e == mempty -> showString "identity"
      | otherwise ->
        showParen (p > 10) $
          showString "perm " . showsPrec 11 (fmap sigma [0 .. (n - 1)])

instance Semigroup Sn where
  Sigma {..} <> Sigma {sigma = sigma', n = n'}
    | isJust n && isJust n' && n /= n' = error "can't multiply elements of uneven length"
    | otherwise = Sigma (sigma . sigma') (n <|> n')

instance Monoid Sn where
  mempty = Sigma id Nothing

instance Group Sn where
  invert = \case
    e@Sigma {n = Nothing} -> e
    Sigma {n = Just n, ..} ->
      perm
        . fmap fst
        . sortOn snd
        $ [0 .. n - 1] `zip` fmap sigma [0 .. n - 1]

instance Eq Sn where
  Sigma {n = Just n, ..} == Sigma {n = Just n', sigma = sigma'}
    | n /= n' = error "can't compare elements of uneven length"
    | otherwise = fmap sigma [0 .. n -1] == fmap sigma' [0 .. n -1]
  Sigma {n = Nothing} == Sigma {n = Nothing} = True
  Sigma {n = Nothing} == Sigma {n = Just n, sigma = sigma} =
    fmap sigma [0 .. n -1] == [0 .. n -1]
  Sigma {n = Just n, sigma = sigma} == Sigma {n = Nothing} =
    fmap sigma [0 .. n -1] == [0 .. n -1]

instance Ord Sn where
  compare = compare `on` getPerm

snElems :: Int -> [Sn]
snElems n = perm <$> permutations [0 .. n]

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
  deriving (Show, Eq, Ord)

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

renderSnLexicographic :: Int -> IO ()
renderSnLexicographic n =
  let es = sort $ snElems n
   in render ("S" ++ show n ++ "-LexicographicOrder") es (greyscaled es)

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
