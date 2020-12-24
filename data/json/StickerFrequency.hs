#!/usr/bin/env stack
{- stack script
    --resolver lts-16.26
    --package aeson
    --package lens
    --package lens-aeson
    --package classy-prelude
    --package diagrams-lib
    --package diagrams-svg
    --package mtl
 -}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Uses facebook gdpr data to generate stats about the most frequent stickers
-- used in a chat.
-- To use this unzip an archive including your messages data so that the
-- messages folder is in the directory from which you run the script and
-- populate the chatIdentifier field, with the name, of the chat, i.e. the
-- subdirectory ./messages/inbox/*
-- Then run the script to generate an SVG, using the default diagrams SVG
-- options.
module Main (main) where

import ClassyPrelude
import Control.Lens
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Lens
import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (at)

countSticker :: MonadState (Map Text Int) m => Text -> m ()
countSticker sticker = modifying (at sticker . non 0) (+ 1)

stickers :: Traversal' Value Text
stickers = key "messages" . values . key "sticker" . key "uri" . _String

-- | given a JSON encoded string from the messenger data count all of the
-- stickers in it
countStickers :: MonadState (Map Text Int) m => Text -> m ()
countStickers = traverseOf_ (_JSON . stickers) countSticker

fetchImage :: FilePath -> IO (Diagram B)
fetchImage fp = scaleUToX 200 . image . fromRight' <$> loadImageEmb fp
  where
    fromRight' (Right x) = x
    fromRight' (Left x) = error $ "couldn't find file corresponding to sticker: " ++ fp

analyzeChat :: String -> FilePath -> IO ()
analyzeChat chatIdentifier outFp = do
  let messagesFp = "messages" </> "inbox" </> chatIdentifier </> "message_1.json"
  jsonText <- readFileUtf8 messagesFp
  let stickersSummary = mapToList $ execState (countStickers jsonText) mempty
  traceShowM $ take 10 $ sortOn (Down . snd) stickersSummary
  stickers <- traverseOf (traverse . _1) (fetchImage . unpack) stickersSummary
  let d = summaryDiagram stickers
  renderSVG outFp (mkHeight 1000) d

toOrdinal :: Int -> String
toOrdinal = \case
  0 -> "0th"
  1 -> "1st"
  2 -> "2nd"
  3 -> "3rd"
  4 -> "4th"
  5 -> "5th"
  n -> error $ "don't know how to turn number into ordinal " ++ show n

placed :: Int -> Diagram B -> Diagram B
placed place d = topLeftText (toOrdinal place) <> d

frequencied :: Int -> Diagram B -> Diagram B
frequencied freq d = bottomRightText (show freq) <> d
  where
    bottomRightText = alignedText 1 0

-- | TODO render a header / footer
summaryDiagram :: [(Diagram B, Int)] -> Diagram B
summaryDiagram stickers = mostUsed
  where
    mostUsed =
      hcat
        . take 3
        -- TODO: make numbers / frequency be redendered in a useful way
        . zipWith placed [1 ..]
        . map (\(d, freq) -> frequencied freq d)
        . sortOn (Down . snd)
        $ stickers

data Args = Args
  { _chatIdentifier :: Text,
    _outFp :: Text
  }

main :: IO ()
main = do
  args <- getArgs
  case Args <$> preview (ix 0) args <*> preview (ix 1) args of
    Nothing ->
      error "must provide chat identifier as first argument and output filepath as second"
    Just (Args chatIdentifier outFp) ->
      analyzeChat (unpack chatIdentifier) (unpack outFp)
