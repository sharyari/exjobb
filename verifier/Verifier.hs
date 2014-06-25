module Verifier where

import Data.Trie as T
import Data.Set as S

import Step
import Gamma
import Alpha
import TrieModule

skriv a = putStrLn(show(a))

verify :: Trie (Set ([String], Bool)) -> Int -> Trie (Set ([String], Bool))
verify trie 0 = trie
verify trie k = let nextIteration = alpha(step(gamma(trie))) in
  if ((getSize nextIteration) == getSize trie) then
    nextIteration
  else
    verify nextIteration (k-1)
