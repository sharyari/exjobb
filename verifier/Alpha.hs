module Alpha where
import Data.Trie as T
import Data.HashSet as S
import Data.Set as S2
import DataTypes
import TrieModule
import StringManipulation
import Data.List as L
import Data.Maybe (fromMaybe, fromJust, isJust)
import Debug.Trace

-- This is a function used for merging the trees.
myFunc a b = Just (S.union a b)


-- This function creates an empty trie, computes adds all new views and adds them to the trie and then
-- merges the two tries to a new trie V.
alpha (trie, seen, list) k =
  let (newTrie, newConfs) = (alpha' T.empty (L.filter (ifSeen trie) $ L.filter (ifSeen seenmmm) list) k []) in
  (T.mergeBy (myFunc) trie newTrie, newConfs, seen)


-- This filters away configurations already seen, avoiding the costly views function. Helps a bit
ifSeen :: CTrie -> C -> Bool
ifSeen _ Null = False
ifSeen trie (Conf state chan) =
  not $ S.member chan (fromMaybe S.empty $ T.lookup state trie)

-- This function checks if a configuration has a certain state
hasState s (Conf state chan) = (s == state)
-- Get functions, returns the evaluation of a configuration
getEval (Conf state chan) = chan

-- This is the actual alpha function. It iterates over the configurations and updates the trie with their views
alpha' :: CTrie -> [C] -> Int -> [C] -> (CTrie,[C])
alpha' trie [] k new = (trie,new)
alpha' trie ((Conf state chan):xs) k new=
  let (relevant',irrelevant) = L.partition (hasState state) ((Conf state chan):xs)
      relevant = L.map getEval relevant'
      (ntrie, new') = (addViews trie state relevant k)
      in
        alpha' ntrie irrelevant k (new'++new)

createConfigurations states eval = (Conf states eval)


-- This function calls addViews' to recursively find all views of the configurations and adds them to the trie
addViews :: CTrie -> State -> [Eval] -> Int -> (CTrie,[C])
addViews trie state chans k =
  let new = addViews' chans k S.empty in
   (tAddList trie state $ new, L.map (createConfigurations state) $ S.toList new)

-- This function finds the views of a function and adds them to a set
addViews' :: [Eval] -> Int -> TNode -> TNode
addViews' [] k node = node
addViews' (x:chans) k node =
  addViews' chans k (S.union node (S.fromList $  views node k x))






