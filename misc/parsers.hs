#!/usr/bin/env stack

{-
  stack script
    --resolver lts-18.6
 -}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Void

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

instance Functor Parser where
  fmap :: forall a b. (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser \s -> case p s of
    Nothing -> Nothing
    Just (s', a) -> Just (s', f a)

instance Applicative Parser where
  pure :: forall a. a -> Parser a
  pure x = Parser \s -> Just (s, x)
  (<*>) :: forall a b. Parser (a -> b) -> Parser a -> Parser b
  (<*>) = ap

joinParser :: forall a. Parser (Parser a) -> Parser a
joinParser (Parser (p :: String -> Maybe (String, Parser a))) = Parser \(s :: String) ->
  case p s of
    Nothing -> Nothing
    Just (s' :: String, Parser (p' :: String -> Maybe (String, a))) -> p' s'

instance Monad Parser where
  p >>= f = joinParser (fmap f p)

instance Alternative Parser where
  empty :: Parser a
  empty = Parser \_ -> Nothing
  (<|>) :: Parser a -> Parser a -> Parser a
  Parser p <|> Parser q = Parser \s -> p s <|> q s

get :: Parser String
get = Parser \s -> Just (s, s)

set :: String -> Parser ()
set s = Parser \_ -> Just (s, ())

assert :: String -> Parser ()
assert s = do
  t <- get
  let slen = length s
      prefix = take slen t
   in if s == prefix
        then set (drop slen t) >> pure ()
        else empty

assert' :: String -> Parser String
assert' s = assert s >> pure s

pBool :: Parser Bool
pBool = (assert "t" $> True) <|> (assert "f" $> False)

pPair :: Parser a -> Parser b -> Parser (a, b)
pPair pA pB = do
  assert "("
  a <- pA
  assert ","
  b <- pB
  assert ")"
  pure (a, b)

pList0 :: Parser [a]
pList0 = assert "[" >> assert "]" >> pure []

pList1 :: Parser a -> Parser [a]
pList1 pA = do
  assert "["
  a <- pA
  assert "]"
  pure [a]

pList2 :: Parser a -> Parser [a]
pList2 pA = do
  assert "["
  a0 <- pA
  assert ","
  a1 <- pA
  assert "]"
  pure [a0, a1]

pList3 :: Parser a -> Parser [a]
pList3 pA = do
  assert "["
  a0 <- pA
  assert ","
  a1 <- pA
  assert ","
  a2 <- pA
  assert "]"
  pure [a0, a1, a2]

commaP :: Parser a -> Parser a
commaP pA = pA <* assert ","

many' :: (Alternative f, Monad f) => f a -> f [a]
many' pA = rec <|> pure []
  where
    rec = do
      a <- pA
      as <- many' pA
      pure (a : as)

pList :: Parser a -> Parser [a]
pList pA = assert "[" *> many' pA <* assert "]"

-- pList :: Parser a -> Parser [a]
-- pList pA = pList0 <|> pList1 pA <|> pList2 pA <|> pList3 pA
