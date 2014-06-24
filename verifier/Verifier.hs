module Verifier where

import Data.Trie as T
import Data.Set as S

import Step
import Gamma
import Alpha

skriv a = putStrLn(show(a))

verify :: Trie (Set ([String], Bool)) -> Int -> Trie (Set ([String], Bool))
verify trie 0 = trie
verify trie k = let nextIteration = alpha(step(gamma(trie))) in
--  if ((T.size nextIteration) == T.size trie) then
--    verify nextIteration
--  else
    verify nextIteration (k-1)
