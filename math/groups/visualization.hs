#!/usr/bin/env stack
{- stack ghci
    --resolver lts-18.6
    --package groups
    --package JuicyPixels
    --package directory
    --package classy-prelude
 -}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import ClassyPrelude
import Codec.Picture
import Data.Monoid (Sum (..))
import System.Directory

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
  render @(Sum Int) "Int-Test" [0, 1, 2]
  pure ()
