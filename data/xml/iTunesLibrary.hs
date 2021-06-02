#!/usr/bin/env stack
{- stack
  script
  --resolver lts-16.23
  --package xml-conduit
  --package bytestring
  --package text
  --package classy-prelude
  --package typed-process
  --package uri-encode
  --package time
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Created for goal of migrating data out of iTunes Library.xml file to beets.
--
-- If you end up in the same unfortunate situation as me where you import all
-- of your music into beets from iTunes and then decide you want to migrate
-- some data from the library for you, this is the thing for you!
--
-- This will for each track in Library.xml in the current directory (produced
-- by doing "File > Library > Export Library" in iTunes/Music
-- * fetch Date Added for all of the music
-- * if the track is Loved in iTunes it will tag it as loved=true
-- * tag the track with the play count
--
-- I had already renamed a bunch of files to match meta data in MusicBrainz
-- unfortunately so I needed to figure out how to connect the tracks together.
-- This uses acoustid_fingerprints in order to match data from your iTunes
-- library, so you should use the fingerprint plugin and use beet fingerprint
-- before running this.
--
-- To use, run with Library.xml via stdin
module Main (main) where

import ClassyPrelude
import Data.Time.Format.ISO8601
import GHC.Stack
import qualified Network.URI.Encode as URI
import System.Exit (ExitCode (..))
import System.Process.Typed
import Text.XML
import qualified Text.XML as XML
import Text.XML.Cursor hiding (bool)

tracks :: Axis
tracks =
  element "dict"
    >=> check (key "Track ID")
    >=> check (key "Track Number")

data TrackInfo = TrackInfo
  { _name :: Text,
    _loved :: Bool,
    _dateAdded :: UTCTime,
    _fingerprint :: Maybe Text,
    _playCount :: Int
  }
  deriving (Eq, Ord)

instance Show TrackInfo where
  show (TrackInfo name loved dateAdded _ playCount) =
    show name ++ " - "
      ++ ifLoved
      ++ "play count "
      ++ show playCount
      ++ ", added on "
      ++ show dateAdded
    where
      ifLoved
        | loved = "❤️ "
        | otherwise = ""

getFingerprint :: HasCallStack => FilePath -> IO (Maybe Text)
getFingerprint path = do
  (errcode, stdout, _) <- readProcess $ proc "fpcalc" [path]
  case errcode of
    ExitSuccess -> do
      let (fingerprintFieldName, splitAt 1 -> (eq, fp)) =
            span (/= '=') $
              indexEx (lines (toStrict (decodeUtf8 stdout))) 1
      unless (fingerprintFieldName == "FINGERPRINT") $
        error "invalid name for FINGERPRINT field"
      unless (eq == "=") $ error "= /= =: i.e. something has gone very wrong"
      pure $ Just fp
    ExitFailure _ -> pure Nothing

data Filter = Filter {_toDrop :: Int, _toTake :: Int, _matches :: Int}
  deriving (Show)

defaultFilters :: [(Int, Int)]
defaultFilters =
  [ -- first short reasonable things that are likely to match for most things
    (50, 50),
    (150, 50),
    -- some shorter things for stuborn matches
    (50, 20),
    (111, 20),
    (150, 20),
    -- some longer things for things that are ambiguous too often
    (0, 250),
    (100, 250)
  ]

-- broadFilters :: [(Int, Int)]
-- broadFilters = take 50 [2, 9 ..] `zip` repeat 20

findGoodFilter :: Text -> IO (Either Text (Maybe Text))
findGoodFilter fp =
  go defaultFilters []
  where
    go [] [] = pure $ Left "no matches with any filter"
    go [] filters =
      pure . Left $
        "several matches"
          ++ " with filters "
          ++ tshow filters
          ++ " for fingerprint "
          ++ fp
    go ((toDrop, toTake) : others) filtersAcc = do
      let filter = "acoustid_fingerprint:" ++ take toTake (drop toDrop fp)
          check = proc "beet" $ map unpack ["ls", filter]
      (out, _) <- readProcess_ check
      case length (lines (decodeUtf8 out)) of
        0 -> go others filtersAcc
        1 -> pure $ Right $ Just filter
        n -> go others (Filter toDrop toTake n : filtersAcc)

beetMod :: TrackInfo -> IO ()
beetMod tinfo@(TrackInfo _ _ _ Nothing _) = sayErr $ "no fingerprint: " ++ tshow tinfo
beetMod tinfo@(TrackInfo _ loved time (Just fp) playCount) = do
  let setLoved = bool Nothing (Just "loved=true") loved
      timeStr = formatTime defaultTimeLocale "%s%Q" time
      setAdded = Just $ "added=" ++ pack timeStr
      setPlayCount = Just $ "playcount=" ++ tshow playCount
  sayErr $ "processing track: " ++ tshow tinfo
  result <- findGoodFilter fp
  case result of
    Left err -> sayErr $ err ++ ": " ++ tshow tinfo
    Right filter -> do
      let command =
            proc "beet" $
              map unpack $
                catMaybes
                  [ Just "mod",
                    Just "-y",
                    filter,
                    setLoved,
                    setAdded,
                    setPlayCount
                  ]
      sayErr $ "executing command: " ++ initEx (tshow command)
      -- swallow the command output instead of also outputting it
      (_, _) <- readProcess_ command
      pure ()

-- | fetch a particular key element that is a child of the cursor
key :: Text -> Axis
key str =
  child
    >=> element "key"
    >=> check (isKey str)
  where
    isKey str x = case (child >=> content) x of
      [str'] -> str == str'
      _ -> False

-- | expects cursor representing a song
isLoved :: HasCallStack => Cursor -> Bool
isLoved =
  toBool
    . (key "Loved" >=> take 1 . following)
  where
    toBool [node -> NodeElement (Element "true" _ _)] = True
    toBool [node -> NodeElement (Element "false" _ _)] = False
    toBool [] = False -- no Loved element means not loved
    toBool _ = error "invalid bool after Loved field"

-- | expects cursor representing a song
contentForKey :: Text -> Cursor -> Text
contentForKey str =
  assertSingleton (unpack str)
    . (key str >=> take 1 . following >=> child >=> content)

maybeContentForKey :: Text -> Cursor -> Maybe Text
maybeContentForKey str =
  listToMaybe
    . (key str >=> take 1 . following >=> child >=> content)

assertSingleton :: (Show a, HasCallStack) => String -> [a] -> a
assertSingleton _ [x] = x
assertSingleton str xs =
  error $
    "not a singleton (while fetching key: " ++ str ++ "): " ++ show xs

toTrackInfo :: HasCallStack => Cursor -> IO TrackInfo
toTrackInfo c = do
  let loved = isLoved c
  let name = contentForKey "Name" c
  let playCount =
        fromMaybe 0 $
          maybeContentForKey "Play Count" c >>= readMay . unpack
  let (Just time) = iso8601ParseM . unpack $ contentForKey "Date Added" c
  -- let filePath = URI.decode . drop 7 . unpack $ contentForKey "Location" c
  let ti = TrackInfo name loved time Nothing playCount
  -- fingerprint <- getFingerprint filePath
  -- pure $ ti {_fingerprint = fingerprint}
  pure ti

-- | given a path to a Library.xml file, fetch TrackInfos from it
getCursors :: FilePath -> IO [Cursor]
getCursors path = do
  document <- XML.readFile def path
  pure $ fromDocument document $// tracks

main :: IO ()
main = do
  cursors <- getCursors "Library.xml"
  for_ cursors $
    toTrackInfo >=> \ti ->
      if _loved ti
        then sayShow ti
        else pure ()
