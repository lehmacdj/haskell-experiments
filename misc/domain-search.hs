#!/usr/bin/env stack
{- stack ghci
    --resolver lts-16.26
    --package classy-prelude
 -}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import ClassyPrelude
import Data.Char (isDigit)
import Data.List (unfoldr)

-- | a domain is a second level domain and a TLD
type Domain = (String, String)

evaluateTld :: String -> Int
evaluateTld = undefined

homerow :: Set Char
homerow = setFromList ['a', 's', 'd', 'f', 'j', 'k', 'l']

keysLeft :: Set Char
keysLeft =
  setFromList $
    ['q', 'w', 'e', 'r', 't', 'a', 's', 'd', 'f', 'g', 'z', 'x', 'c', 'v', 'b']
      ++ ['1', '2', '3', '4', '5']

keysRight :: Set Char
keysRight =
  setFromList $
    ['y', 'u', 'i', 'o', 'p', 'h', 'j', 'k', 'l', 'n', 'm', 'm']
      ++ ['6', '7', '8', '9', '0', '-']

vowels :: Set Char
vowels = setFromList ['a', 'e', 'i', 'o', 'u']

consonants :: Set Char
consonants = setFromList ['a' .. 'z'] \\ vowels

test_keysLeftRightAreAlphabet :: IO ()
test_keysLeftRightAreAlphabet =
  if keysLeft <> keysRight /= setFromList (['a' .. 'z'] ++ ['0' .. '9'])
    then error "keysLeft and keysRight don't cover the alphabet"
    else pure ()

isKeyLeft :: Char -> Bool
isKeyLeft = (`member` keysLeft)

isKeyRight :: Char -> Bool
isKeyRight = (`member` keysRight)

isHomeRow :: Char -> Bool
isHomeRow = (`member` homerow)

isVowel :: Char -> Bool
isVowel = (`member` vowels)

isConsonant :: Char -> Bool
isConsonant = (`member` consonants)

-- | all adjacent pairs of elements in a list
pairwise :: [a] -> [(a, a)]
pairwise [] = []
pairwise [_] = []
pairwise (x : y : ys) = (x, y) : pairwise (y : ys)

areDifferent :: (a -> Bool) -> (a -> Bool) -> ((a, a) -> Bool)
areDifferent p q (x, y) = p x && q y || q x && p y

-- 2 per pair of letters next to one another that are not typed by the same hand
-- 2 per last letter not typed with left hand because next letter will always
-- be a period
-- 1 point per non-homerow key
typeabilityScore :: String -> Int
typeabilityScore str = nonHomerowPenalty + nonOppositePenalty + rightLastPenalty
  where
    nonHomerowPenalty = sum . map (bool 1 0 . isHomeRow) $ str
    nonOppositePenalty =
      sum . map (bool 2 0 . areDifferent isKeyLeft isKeyRight) . pairwise $ str
    rightLastPenalty = case lastMay str of
      Just c
        | isKeyRight c -> 2
        | otherwise -> 0
      Nothing -> 0

-- 1 point for being two vowels or consonants in sequence, so that sequences
-- that alternate vowels and consonants are benefitted
pronouceabilityScore :: String -> Int
pronouceabilityScore =
  sum . map (bool 2 0 . areDifferent isVowel isConsonant) . pairwise

-- number keys are hard to type so a penalty of 3 for each number
numberKeyPenalty :: String -> Int
numberKeyPenalty = sum . map (bool 0 3 . isDigit)

dashPenalty :: String -> Int
dashPenalty = sum . map (bool 0 3 . (== '-'))

evaluateName :: String -> Int
evaluateName =
  map sum $
    sequence
      [ typeabilityScore,
        pronouceabilityScore,
        numberKeyPenalty
      ]

applyN :: forall a. Int -> (a -> a) -> a -> a
applyN n f = foldr @[a -> a] (.) id (replicate n f)

domainNamesByLength :: [[String]]
domainNamesByLength =
  unfoldr (\n -> Just (applyN n (<**> addNewChar) [""], n + 1)) 1
  where
    addNewChar = map (\x -> (++ singleton x)) $ ['0' .. '9'] ++ ['a' .. 'z']

sortedDomainNamesByLength :: [[String]]
sortedDomainNamesByLength = map (sortOn evaluateName) domainNamesByLength

main :: IO ()
main = undefined
