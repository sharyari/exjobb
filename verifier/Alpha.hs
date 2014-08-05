module Alpha where
import Data.Trie as T
import Data.HashSet as S

import Data.Set as S2


import DataTypes
import TrieModule
import StringManipulation
import Data.ByteString as B
import Data.ByteString.Char8 as B2
import Data.List as L
import Data.Maybe (fromMaybe, fromJust, isJust)
import Debug.Trace


-- Alpha takes a list of configurations and a trie, and adds all the views of the configurations in the trie


-- This is a function used for merging the trees. For some reason, it must be of type maybe
myFunc a b = Just (S.union a b)



-- This filters away configurations already seen, avoiding the costly views function. Helps a bit
ifSeen :: CTrie -> C -> Bool
ifSeen _ Null = False
ifSeen trie (Conf state chan) = let states = T.lookup state trie in
  not $ S.member chan (fromMaybe S.empty states)

-- This function creates an empty trie, adds all new configurations to the trie and then
-- merges the two tries to a new one.
alpha (trie, seen, list) k = (T.mergeBy (myFunc) trie $ (alpha' T.empty (L.filter (ifSeen seen) list) k), seen)



findSameState s (Conf state chan) = (s == state)
getChan (Conf state chan) = chan

-- This is the actual alpha function. It iterates over the configurations and updates the trie with their views
alpha' :: CTrie -> [C] -> Int -> CTrie
alpha' trie [] k = trie
alpha' trie ((Conf state chan):xs) k =
  let (relevant',irrelevant) = L.partition (findSameState state) ((Conf state chan):xs)
      relevant = L.map getChan relevant' in
        alpha' (addViews trie state relevant k) irrelevant k

-- This function creates the views of a single configuration, and adds it to the trie
addViews :: CTrie -> ByteString -> [[ByteString]] -> Int -> CTrie
addViews trie state chans k =
  let
    node = fromMaybe S.empty $ T.lookup state trie 
  in
   helpFunction trie state k $ addViews' trie state chans k S.empty

addViews' trie state [] k node = node
addViews' trie state (x:chans) k node =
  addViews' trie state chans k (S.union node (S.fromList $  views node k x))

bla2 a b = S.member b a

helpFunction trie state k node =
  tAddList trie state node





