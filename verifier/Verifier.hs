module Verifier where

import Data.Trie as T
import Data.Set as S

import Step
import Gamma
import Alpha
import TrieModule
import DataTypes

skriv a = putStrLn(show(a))

-- Idea: First do gamma without looking for longer words, until the lists are equal length
-- Then take longer words once, and go back to not doing this. When both versions return equal length, quit

verify :: (CTrie, CTrie) -> Trie [R] -> Int -> Int -> CTrie
verify (trie,seen) rules _ 0 = trie
verify (trie,seen) rules k c = let nextIteration = alpha (step (gamma (trie,seen) k) rules ) k in
  if ((getSize (fst nextIteration)) == getSize trie) then
    fst   nextIteration
  else
    verify nextIteration rules k (c-1)
