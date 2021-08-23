#!/usr/bin/env stack
{- stack ghci
    --resolver lts-18.6
    --package groups
    --package JuicyPixels
    --package directory
    --package classy-prelude
 -}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import ClassyPrelude
import Codec.Picture
import Data.Group
import Data.Monoid (Sum (..))
import GHC.Stack (HasCallStack)
import System.Directory
import Prelude (showString, showParen)

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
    Sigma {n=Nothing, ..} -> showString "identity"
    Sigma {n=Just n, ..} -> showParen (p > 10) $ 
      showString "perm " . showsPrec 11 (fmap sigma [0 .. (n - 1)])

instance Semigroup Sn where
  Sigma {..} <> Sigma {sigma=sigma', n=n'}
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
  Sigma {n = Just n, ..} == Sigma {n = Just n', sigma=sigma'}
    | n /= n' = error "can't compare elements of uneven length"
    | otherwise = fmap sigma [0 .. n-1] == fmap sigma' [0 .. n-1]
  Sigma {n = Nothing} == Sigma {n = Nothing} = True
  Sigma {n = Nothing} == Sigma {n = Just n, sigma = sigma}
    = fmap sigma [0 .. n-1] == [0 .. n-1]
  Sigma {n = Just n, sigma = sigma} == Sigma {n = Nothing}
    = fmap sigma [0 .. n-1] == [0 .. n-1]

s4Elems :: [Sn]
s4Elems = perm <$> permutations [0 .. 3]

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

instance Colorable Z2 where
  colorOf Zero = PixelRGBA16 0 0 0 0
  colorOf _ = PixelRGBA16 maxBound maxBound maxBound maxBound

class Colorable m where
  colorOf :: m -> PixelRGBA16

mkImage :: (Pixel px, Monoid m) => [m] -> (m -> px) -> Image px
mkImage es toColor = generateImage pixelAt width height
  where
    width = length es
    height = width
    pixelAt x y = toColor $ (es `indexEx` x) <> (es `indexEx` y)

ensureDirExists :: IO ()
ensureDirExists = createDirectoryIfMissing True "images"

pathOfName :: String -> String -> FilePath
pathOfName name extension = "images/" ++ name ++ "." ++ extension

render :: (Colorable m, Monoid m) => FilePath -> [m] -> IO ()
render name es = do
  ensureDirExists
  let image = mkImage es colorOf
  writePng (pathOfName name "png") image

instance Colorable (Sum Int) where
  colorOf (Sum 0) = PixelRGBA16 0 0 0 0
  colorOf _ = PixelRGBA16 maxBound maxBound maxBound maxBound

main :: IO ()
main = do
  render "Z2-Test" [Zero, One]
  -- render "S4-LexicographicOrder" s4Elems
  pure ()
