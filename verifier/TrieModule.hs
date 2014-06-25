module TrieModule where
import DataTypes

import Data.Trie as T
import Data.Set as S
import Data.List as L
import Data.ByteString
import Data.Maybe (fromMaybe, fromJust, isJust)
-- Tries are used to store configurations and to store rules, the latter is static
-- Storing the rules in a trie allows for efficiently finding the relevant rules at each point


------------------------------------------------------
--------------- SECTION CONFIGURATIONS ---------------
------------------------------------------------------
-- ([String], Bool)

--This function adds a configuration to the trie
tAdd :: CTrie -> C -> Bool -> CTrie
tAdd trie (Conf key val) seen = let s = T.lookup key trie in
    T.insert key (S.insert (val,seen) (fromMaybe (S.empty) s)) trie

tAddList :: CTrie -> ByteString -> Set ([String], Bool) -> Bool -> CTrie
tAddList trie key list seen = let s = T.lookup key trie in
    T.insert key ( S.union (fromMaybe (S.empty) s) list) trie

-- foldl, foldr, does it matter?
getSize trie = L.foldl (+) 0 (L.map S.size (L.map snd (T.toList trie)))

------------------------------------------------------
------------------- SECTION RULES --------------------
------------------------------------------------------


tAddRule trie (key,newState,chmod) = let s = T.lookup key trie in
  if (isJust s) then
    T.insert key ((Rule newState chmod):fromJust s) trie
  else T.insert key ([Rule newState chmod]) trie

tAddRuleList trie [] = trie
tAddRuleList trie (h:l) = tAddRuleList (tAddRule trie h) l
