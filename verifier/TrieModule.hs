module TrieModule where
import DataTypes

import Data.Trie as T
import Data.HashSet as S
import Data.HashMap.Strict as M
import Data.List as L
import Data.ByteString as B
import Data.Maybe (fromMaybe, fromJust, isJust)



-- Tries are used to store configurations and to store rules, the latter is static
-- Storing the rules in a trie allows for efficiently finding the relevant rules at each point


------------------------------------------------------
--------------- SECTION CONFIGURATIONS ---------------
------------------------------------------------------
-- ([String], Bool)

--This function adds a configuration to the trie
tAdd :: CTrie -> C ->  CTrie
tAdd trie (key, val) =
    M.insert key (S.insert val $ fromMaybe S.empty (M.lookup key trie)) trie

tAdd2 trie  [] = trie
tAdd2 trie (c:onfs) = tAdd2 (tAdd trie c) onfs

tAddList :: CTrie -> B.ByteString -> TNode -> CTrie
tAddList trie key list =
    M.insert key (S.union list $ fromMaybe S.empty (M.lookup key trie)) trie

tChange trie key node =
  M.insert key node trie

getSize trie = L.foldl (+) 0 $ L.map (S.size . snd) $ M.toList trie


findStateInTrie state trie = fromMaybe S.empty $ M.lookup state trie


-- This filters away configurations already seen, avoiding the costly views function. Helps a bit
ifSeen :: CTrie -> C -> Bool
ifSeen trie (state, chan) =
  not $ S.member chan (fromMaybe S.empty $ M.lookup state trie)

------------------------------------------------------
------------------- SECTION RULES --------------------
------------------------------------------------------

tAddRule trie (key,newState,chmod) = let s = T.lookup key trie in
  if (isJust s) then
    T.insert key ((Rule newState chmod):fromJust s) trie
  else T.insert key ([Rule newState chmod]) trie

tAddRuleList trie [] = trie
tAddRuleList trie (h:l) = tAddRuleList (tAddRule trie h) l
