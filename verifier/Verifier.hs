module Verifier where

import Data.Trie as T
import Data.Set as S

import Step
import Gamma
import Alpha
import TrieModule
import DataTypes

skriv a = putStrLn(show(a))


-- This is the verifier, and it basically iterates alpha $ step $ gamma
-- It is divided into two almost identical functions:
-- If gamma gets false, it does the cheaper operation of only stepping
-- If gamma gets true, it creates the longer words. As long as possible, only step
verify :: (CTrie, CTrie) -> Trie [R] -> Int -> Int -> CTrie
verify (trie,seen) rules _ 0 = trie
verify (trie,seen) rules k c = let nextIteration = alpha (step (gamma (trie,seen) k False ) rules ) k in
  if ((getSize (fst nextIteration)) == getSize trie) then
    verify2 nextIteration rules k (c-1)
  else
    verify nextIteration rules k (c-1)

verify2 :: (CTrie, CTrie) -> Trie [R] -> Int -> Int -> CTrie
verify2 (trie,seen) rules _ 0 = trie
verify2 (trie,seen) rules k c = let nextIteration = alpha (step (gamma (trie,seen) k True ) rules ) k in
  if ((getSize (fst nextIteration)) == getSize trie) then
    fst nextIteration	    
  else
    verify nextIteration rules k (c-1)
