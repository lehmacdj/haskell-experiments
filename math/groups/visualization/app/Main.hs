{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import Data.Group
import Data.Monoid (Sum (..))
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
  compare = compare `on` show

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

mkImage :: forall m px. (Pixel px, Show m, Monoid m, Ord m) => [(m, px)] -> Image px
mkImage es = generateImage pixelAt width height
  where
    cmap :: Map m px
    cmap = mapFromList es
    toColor m =
      fromMaybe (error ("don't have color for " ++ show m)) $ lookup m cmap
    width = length es
    height = width
    es' = fmap fst es
    pixelAt x y = toColor $ (es' `indexEx` x) <> (es' `indexEx` y)

ensureDirExists :: IO ()
ensureDirExists = createDirectoryIfMissing True "images"

pathOfName :: String -> String -> FilePath
pathOfName name extension = "images/" ++ name ++ "." ++ extension

render ::
  (Pixel px, PngSavable px, Show m, Monoid m, Ord m) =>
  FilePath ->
  [(m, px)] ->
  IO ()
render name es = do
  ensureDirExists
  let image = mkImage es
  writePng (pathOfName name "png") image

black, white :: PixelRGBA16
black = PixelRGBA16 0 0 0 maxBound
white = PixelRGBA16 maxBound maxBound maxBound maxBound

greyscaled :: [a] -> [(a, Pixel16)]
greyscaled xs =
  runIdentity $
    itraverse
      ( \i x ->
          Identity
            ( x,
              let frac = fromIntegral i / fromIntegral len
                  full = fromIntegral (maxBound :: Word16) :: Double
               in round (frac * full)
            )
      )
      xs
  where
    len
      | null xs = 1
      | otherwise = length xs - 1

renderSnLexicographic :: Int -> IO ()
renderSnLexicographic n =
  render ("S" ++ show n ++ "-LexicographicOrder") (greyscaled . sort $ snElems n)

main :: IO ()
main = do
  renderSnLexicographic 5
  pure ()
