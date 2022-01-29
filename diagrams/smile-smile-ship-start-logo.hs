#!/usr/bin/env cabal
{- cabal:
build-depends:
  , base
  , classy-prelude
  , diagrams-contrib
  , diagrams-core
  , diagrams-lib
  , diagrams-svg
  , generic-lens
  , lens
  , mtl
  , random
  , text
-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | This generates a graphic that looks like the 5 pointed pink and light blue
-- star shaped logo found on the shirts of Chika, Ruby, and Kanan in Smile
-- Smile Ship Start by Aqours.
module Main (main, testTargets) where

import ClassyPrelude
import Control.Arrow ((>>>))
import Control.Lens
import Data.List (cycle, iterate)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Diagrams.Angle ((@@))
import Diagrams.Backend.SVG qualified as D
import Diagrams.Backend.SVG.CmdLine qualified as D
import Diagrams.Prelude (ColourOps)
import Diagrams.Prelude qualified as D
import Diagrams.TwoD.Polygons qualified as D

type Color = D.Colour Double

type Diagram = D.Diagram D.B

data ColorPalette = ColorPalette
  { border :: Color,
    -- | First color for the background pattern (on the outside / most massive part)
    patternPrimary :: Color,
    -- | Second color for the background pattern (on the inside)
    patternSecondary :: Color,
    background :: Color
  }
  deriving (Show, Eq, Generic)

pink :: Color
pink = D.sRGB24 235 78 134

cyan :: Color
cyan = D.sRGB24 108 218 230

bone :: Color
bone = D.sRGB24 251 252 255

grey :: Color
grey = D.sRGB24 130 140 160

navy :: Color
navy = D.sRGB24 33 53 91

frozenTurquoise :: Color
frozenTurquoise = D.sRGB24 140 231 251

belowZero :: Color
belowZero = D.sRGB24 148 209 237

blueMana :: Color
blueMana = D.sRGB24 148 209 237

magicalMerlin :: Color
magicalMerlin = D.sRGB24 90 150 202

smileSmileShipStartColorPalette, fantasticDeparturesColorPalette :: ColorPalette
smileSmileShipStartColorPalette = ColorPalette pink magicalMerlin frozenTurquoise D.white
fantasticDeparturesColorPalette = ColorPalette cyan bone grey navy

sandDollar ::
  -- | color for outer part
  Color ->
  -- | color for inner part
  Color ->
  Diagram
sandDollar primary secondary = birds <> petals
  where
    birds = foldMap (`D.rotate` D.translateY (-0.9) bird) angles
    birdOuterRight =
      D.lineFromSegments
        [ D.bezier3 (D.V2 0 (-0.3)) (D.V2 0.5 (-0.7)) (D.V2 1 (-0.77)),
          D.bezier3 (D.V2 (-0.3) (-0.1)) (D.V2 (-0.7) (-0.2)) (D.V2 (-1) 0.22)
          -- D.straight (D.V2 (-0.95) 0.4)
        ]
    birdOuter =
      D.toPath . D.glueLine $
        birdOuterRight <> D.reverseLine (D.reflectX birdOuterRight)
    birdInnerRight =
      D.lineFromSegments
        [ D.bezier3 (D.V2 0.1 (-0.15)) (D.V2 0.35 (-0.38)) (D.V2 0.63 (-0.5)),
          D.bezier3 (D.V2 0.10 0.05) (D.V2 (-0.3) (-0.1)) (D.V2 (-0.63) 0.36)
        ]
    birdInner =
      D.translateY (-0.26) . D.toPath . D.glueLine $
        birdInnerRight <> D.reverseLine (D.reflectX birdInnerRight)
    bird =
      -- ((D.stroke birdInnerApprox & D.lineColor D.red) <>) $
      D.stroke (birdOuter <> D.reversePath birdInner)
        & D.fillColor primary
        & D.lineColor D.transparent
        & D.lineWidth 0
    petals = foldMap (`D.rotate` petal) angles
    petalLeft = D.bezier3 (D.V2 (-0.6) 0.8) (D.V2 (-0.5) 1.1) (D.V2 0 2)
    petalOutline =
      D.glueLine $
        D.lineFromSegments [petalLeft, D.reverseSegment (D.reflectX petalLeft)]
    petalInner =
      petalOutline
        & D.scale 0.6
        & D.stroke
        & D.translateY 0.2
        & D.lineColor D.transparent
        & D.lineWidth 0
        & D.fillColor secondary
        & D.clipBy (D.translateY (-0.5) (D.circle 1))
    petal =
      D.stroke petalOutline
        & D.lineColor secondary
        & D.lineWidth 3
        & D.atop petalInner
    angles = [0, 0.2 .. 0.8] <&> (@@ D.turn)

cycleN :: Int -> [a] -> [a]
cycleN n = concat . asList . replicate n

backgroundPattern :: Color -> Color -> Diagram
backgroundPattern primary secondary =
  sandDollar primary secondary
    & replicate 7
    & D.hsep 0.3
    & replicate 7
    & D.vsep 0.3
    & D.center
    & D.clipBy (starTransformation outerStarPath)

starTransformation ::
  ( D.Alignable t,
    D.HasOrigin t,
    D.Transformable t,
    Eq (D.N t),
    Floating (D.N t),
    D.V t ~ D.V2
  ) =>
  t ->
  t
starTransformation = D.center . D.scale 6 . D.rotate (-0.13 @@ D.turn)

outerStarPath :: D.Path D.V2 Double
outerStarPath = D.toPath $ D.polySidesTrail angles lengths
  where
    angles = cycleN 5 $ [0.125, 0.25, 0.125, -0.3] <&> (@@ D.turn)
    lengths = initEx $ cycleN 5 [0.7, 1, 1, 0.7]

mainStar :: Color -> Diagram
mainStar color = addRays outerStar <> innerStar & starTransformation
  where
    outerStar :: Diagram
    outerStar =
      outerStarPath
        & D.stroke'
          (D.with & D.vertexNames .~ [[0 :: Int ..]])
        & D.lineColor color
        & D.lineWidth 40
    innerStar :: Diagram
    innerStar =
      D.stroke (D.polyPolarTrail angles radii)
        & D.fillColor color
        & D.lineColor color
        & D.rotate (0.75 @@ D.turn)
      where
        angles = cycle $ [0.05, 0.10, 0.05] <&> (@@ D.turn)
        radii = cycleN 5 [0.8, 0.3, 0.3]
    addRays :: Diagram -> Diagram
    addRays =
      D.withNames @Int [2, 6 .. 18] $
        D.atop
          . mconcat
          . fmap
            ( \starPoint ->
                D.location starPoint D.~~ D.origin
                  & D.lineColor color
                  & D.lineWidth 35
            )

diagram :: ColorPalette -> Diagram
diagram ColorPalette {..} =
  -- TODO: make sandDollars into whole background
  (mainStar border & D.center) <> backgroundPattern patternPrimary patternSecondary
    & D.pad 1.1
    & D.bg background

render :: String -> Diagram -> IO ()
render name = D.renderSVG ("images/" ++ name ++ ".svg") (D.mkHeight 1000)

autoReload :: Maybe Int
autoReload = Nothing

-- render a diagram that auto-reloads by injecting javascript into the end of
-- the svg
renderAutoreloading :: String -> Diagram -> IO ()
renderAutoreloading name d = do
  render name d
  for_ autoReload $ \delaySeconds -> do
    let fn = "images/" ++ name ++ ".svg"
    contents <- T.readFile fn
    T.writeFile fn $
      fromMaybe (error "bad svg file!") (T.stripSuffix "</svg>" contents)
        <> "<script>setTimeout(() => location.reload(), "
        <> tshow (delaySeconds * 1000)
        <> ")</script></svg>"

testSandDollar, testStar, testFantasticDepartures, testSmileSmileShipStart :: IO ()
testSandDollar =
  renderAutoreloading "sand-dollar" $
    sandDollar bone grey & D.pad 1.1 & D.bg navy
testStar =
  renderAutoreloading "star" $
    mainStar pink & D.pad 1.1
testFantasticDepartures =
  renderAutoreloading "fantastic-departures" $
    diagram fantasticDeparturesColorPalette
testSmileSmileShipStart =
  renderAutoreloading "smile-smile-ship-start" $
    diagram smileSmileShipStartColorPalette

testTargets :: [IO ()]
testTargets =
  [ testSandDollar,
    testStar,
    testFantasticDepartures,
    testSmileSmileShipStart
  ]

{- ORMOLU_DISABLE -}
-- The below comment makes it so that ghcid will regenerate stuff automatically.
-- $> sequence testTargets
{- ORMOLU_ENABLE -}

main :: IO ()
main =
  D.mainWith
    [ ("Smile Smile Ship Start!" :: String, diagram smileSmileShipStartColorPalette),
      ("Fantastic Departures", diagram fantasticDeparturesColorPalette)
    ]
