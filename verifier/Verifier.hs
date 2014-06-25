module Verifier where

import Data.Trie as T
import Data.Set as S

import Step
import Gamma
import Alpha
import TrieModule
import DataTypes

skriv a = putStrLn(show(a))

verify :: CTrie -> Trie [R] -> Int -> Int -> CTrie
verify trie rules _ 0 = trie
verify trie rules k c = let nextIteration = alpha (step (gamma trie k) rules ) k in
  if ((getSize nextIteration) == getSize trie) then
    nextIteration
  else
    verify nextIteration rules k (c-1)
