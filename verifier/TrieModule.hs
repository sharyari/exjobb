module TrieModule where
import DataTypes

import Data.Trie as T
import Data.HashSet as S
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
tAdd trie Null = trie
tAdd trie (Conf key val) =
    T.insert key (S.insert val $ fromMaybe S.empty (T.lookup key trie)) trie

tAdd2 trie  [] = trie
tAdd2 trie (c:onfs) = tAdd2 (tAdd trie c) onfs

tAddList :: CTrie -> B.ByteString -> TNode -> CTrie
tAddList trie key list =
    T.insert key (S.union list $ fromMaybe S.empty (T.lookup key trie)) trie

getSize trie = L.foldl (+) 0 $ L.map (S.size . snd) $ T.toList trie


findStateInTrie state trie = fromMaybe S.empty $ T.lookup state trie


-- This filters away configurations already seen, avoiding the costly views function. Helps a bit
ifSeen :: CTrie -> C -> Bool
ifSeen _ Null = False
ifSeen trie (Conf state chan) =
  not $ S.member chan (fromMaybe S.empty $ T.lookup state trie)

------------------------------------------------------
------------------- SECTION RULES --------------------
------------------------------------------------------

tAddRule trie (key,newState,chmod) = let s = T.lookup key trie in
  if (isJust s) then
    T.insert key ((Rule newState chmod):fromJust s) trie
  else T.insert key ([Rule newState chmod]) trie

tAddRuleList trie [] = trie
tAddRuleList trie (h:l) = tAddRuleList (tAddRule trie h) l
