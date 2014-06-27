module TrieModule where
import DataTypes

import Data.Trie as T
import Data.HashSet as S
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
tAdd :: CTrie -> C ->  CTrie
tAdd trie (Conf key val) = let s = T.lookup key trie in
    T.insert key (S.insert val (fromMaybe (S.empty) s)) trie

tAdd2 trie  [] = trie
tAdd2 trie (c:onfs) = tAdd2 (tAdd trie c) onfs

-- This is slow and cannot get faster, the union is the culprit
tAddList :: CTrie -> ByteString -> TNode -> CTrie
tAddList trie key list = let s = T.lookup key trie in
    T.insert key ( S.union list (fromMaybe (S.empty) s)) trie

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
