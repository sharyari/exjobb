module TrieModule where
import DataTypes

import Data.Trie as T
import Data.Set as S
import Data.Maybe (fromMaybe, fromJust, isJust)
-- Tries are used to store configurations and to store rules, the latter is static
-- Storing the rules in a trie allows for efficiently finding the relevant rules at each point


------------------------------------------------------
--------------- SECTION CONFIGURATIONS ---------------
------------------------------------------------------
-- ([String], Bool)

--This function adds a configuration to the trie
tAdd :: Trie (Set ([String], Bool)) -> C -> Bool -> Trie (Set ([String], Bool))
tAdd trie (Conf key val) seen = let s = T.lookup key trie in
    T.insert key (S.insert (val,seen) (fromMaybe (S.empty) s)) trie

--This function adds a list of configurations to the trie
tAddList :: Trie (Set ([String], Bool)) -> [C] -> Bool -> Trie (Set ([String], Bool))
tAddList trie [] seen = trie
tAddList trie (x:xs) seen = tAddList (tAdd trie x seen) xs seen



------------------------------------------------------
------------------- SECTION RULES --------------------
------------------------------------------------------


tAddRule trie (key,newState,chmod) = let s = T.lookup key trie in
  if (isJust s) then
    T.insert key ((Rule newState chmod):fromJust s) trie
  else T.insert key ([Rule newState chmod]) trie

