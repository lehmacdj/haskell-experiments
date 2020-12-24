#!/usr/bin/env stack
{- stack
  script
  --resolver lts-16.23
  --package xml-conduit
  --package bytestring
  --package text
-}

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Text as T
import Text.XML
import Text.XML.Cursor

extractDocumentTitles :: Document -> [String]
extractDocumentTitles document =
  fromDocument document $// hasAttribute "title" >=> map T.unpack . attribute "title"

extractOpmlTitles :: String -> String
extractOpmlTitles input = case parseLBS def (LBS.pack input) of
  Left err -> "Couldn't parse input to XML\n" ++ show err
  Right doc -> unlines $ extractDocumentTitles doc

main :: IO ()
main = interact extractOpmlTitles
