module Main where

import ClassyPrelude
import Lib

main :: IO ()
main = do
  renderZ 10
  renderSymLexicographic 4
  renderFree [True, False] 50
  renderDihedral 6
