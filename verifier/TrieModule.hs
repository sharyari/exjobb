module TrieModule where
import DataTypes

import Data.Trie as T
import Data.HashSet as S
import Data.HashMap.Strict as M
import Data.Maybe (fromMaybe, fromJust, isJust)



------------------------------------------------------
--------------- SECTION CONFIGURATIONS ---------------
------------------------------------------------------

-- This function add a configuration to the correct node in a hashmap
mapAdd :: CMap -> C ->  CMap
mapAdd hmap (key, val) =
    M.insert key (S.insert val $ fromMaybe S.empty (M.lookup key hmap)) hmap

-- This function mearly adds a list of configurations to a hashmap
-- If the configurations are same-state, use mapAddList instead
mapAddList hmap  [] = hmap
mapAddList hmap (c:onfs) = mapAddList (mapAdd hmap c) onfs

-- This function adds a node to a Hashmap
mapAddNode :: CMap -> State -> MapNode -> CMap
mapAddNode hmap key list =
    M.insert key (S.union list $ fromMaybe S.empty (M.lookup key hmap)) hmap

-- This function returns the accumulated size of the nodes in a hashmap, ie. the
-- number of configurations.
getSize hmap = Prelude.foldl (+) 0 $ Prelude.map (S.size . snd) $ M.toList hmap

-- This function returns the node of a hashmap given a key.
findNodeInMap state hmap = fromMaybe S.empty $ M.lookup state hmap


-- This filters away configurations already seen, avoiding the costly views function.
ifSeen :: CMap -> C -> Bool
ifSeen hmap (state, chan) =
  not $ S.member chan (fromMaybe S.empty $ M.lookup state hmap)

------------------------------------------------------
------------------- SECTION RULES --------------------
------------------------------------------------------
-- This function add a rule to a rule-trie
tAddRule :: RuleMap -> (State,State, (Int, String, CWord)) -> RuleMap
tAddRule rmap (key, newState, chmod) =
    M.insert key ((newState, chmod):(fromMaybe [] $ M.lookup key rmap)) rmap

-- This function adds a list of rules to a trie
tAddRuleList :: RuleMap -> [(State,State, (Int, String, CWord))] -> RuleMap
tAddRuleList rmap [] = rmap
tAddRuleList rmap (h:l) = tAddRuleList (tAddRule rmap h) l

-- This function returns the node of a trie given a key.
findNodeInTrie state hmap = fromMaybe [] $ M.lookup state hmap
