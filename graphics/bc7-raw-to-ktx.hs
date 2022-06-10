#!/usr/bin/env stack
{- stack script
    --resolver lts-18.28
    --package classy-prelude
    --package bytestring
 -}

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Embeds a raw stream of BC7 data into this container:
-- https://www.khronos.org/registry/KTX/specs/1.0/ktxspec_v1.html
module Main (main) where

import ClassyPrelude
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder
import Data.ByteString.Lazy qualified as LByteString
import System.Exit (exitFailure)

class BinaryEncode a where
  intoBuilder :: a -> ByteStringBuilder

intoBytes :: BinaryEncode a => a -> LByteString
intoBytes = builderToLazy . intoBuilder

data Ktx11Bptc = Ktx11Bptc
  { bptcVariant :: BptcVariant,
    pixelHeight :: Word32,
    pixelWidth :: Word32,
    binaryData :: ByteString
  }
  deriving (Show)

instance BinaryEncode Ktx11Bptc where
  intoBuilder Ktx11Bptc {..} =
    byteString ktx11Identifier
      <> word32LE endiannessMarker
      <> word32LE 0 -- glType
      <> word32LE 1 -- glTypeSize
      <> word32LE 0 -- glFormat
      <> intoBuilder bptcVariant -- glInternalFormat
      <> word32LE 0 -- glBaseInternalFormat (<!> 0 taken from example not docs)
      <> word32LE pixelWidth
      <> word32LE pixelHeight
      <> word32LE 0 -- pixelDepth
      <> word32LE 0 -- numberOfArrayElements
      <> word32LE 1 -- numberOfFaces
      <> word32LE 1 -- numberOfMipmapLevels
      <> word32LE 0 -- bytesOfKeyValueData
      <> word32LE (fromIntegral $ length binaryData)
      <> byteString binaryData

endiannessMarker :: Word32
endiannessMarker = 0x04030201

ktx11Identifier :: ByteString
ktx11Identifier = "\xAB\x4B\x54\x58\x20\x31\x31\xBB\x0D\x0A\x1A\x0A"

data BptcVariant
  = COMPRESSED_RGBA_BPTC_UNORM
  | COMPRESSED_SRGB_ALPHA_BPTC_UNORM
  | COMPRESSED_RGBA_BPTC_SIGNED_FLOAT
  | COMPRESSED_SRGB_ALPHA_BPTC_SIGNED_FLOAT
  deriving (Show)

instance BinaryEncode BptcVariant where
  intoBuilder = \case
    COMPRESSED_RGBA_BPTC_UNORM -> word32LE 0x8E8C
    COMPRESSED_SRGB_ALPHA_BPTC_UNORM -> word32LE 0x8E8D
    COMPRESSED_RGBA_BPTC_SIGNED_FLOAT -> word32LE 0x8E8E
    COMPRESSED_SRGB_ALPHA_BPTC_SIGNED_FLOAT -> word32LE 0x8E8F

data Args = Args
  { width :: Word32,
    height :: Word32
  }

parseArgs :: [Text] -> Maybe Args
parseArgs [width, height] = Args <$> readMay width <*> readMay height
parseArgs _ = Nothing

onNothingM :: Monad m => m a -> m (Maybe a) -> m a
onNothingM err = (>>= maybe err pure)

fatalErr :: Text -> IO a
fatalErr msg = do
  sayErr $ "error: " <> msg
  sayErr "usage: ./bc7-raw-to-ktx.hs {width} {height} <{in.raw} >{out.ktx}"
  exitFailure

main :: IO ()
main = do
  Args {width, height} <-
    onNothingM (fatalErr "couldn't parse command line args") $
      parseArgs <$> getArgs
  rawData <- ByteString.getContents
  LByteString.putStr . intoBytes $
    Ktx11Bptc
      { bptcVariant = COMPRESSED_RGBA_BPTC_UNORM,
        pixelHeight = height,
        pixelWidth = width,
        binaryData = rawData
      }
