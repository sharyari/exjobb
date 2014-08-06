module Alpha where
import Data.Trie as T
import Data.HashSet as S
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
alpha (trie, seen, list) k = (T.mergeBy (myFunc) trie $ (alpha' T.empty (L.filter (ifSeen seen)  list) k), seen)


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
alpha' :: CTrie -> [C] -> Int -> CTrie
alpha' trie [] k = trie
alpha' trie ((Conf state chan):xs) k =
  let (relevant',irrelevant) = L.partition (hasState state) ((Conf state chan):xs)
      relevant = L.map getEval relevant' in
        alpha' (addViews trie state relevant k) irrelevant k

-- This function calls addViews' to recursively find all views of the configurations and adds them to the trie
addViews :: CTrie -> CWord -> [Eval] -> Int -> CTrie
addViews trie state chans k =
   tAddList trie state $ addViews' chans k S.empty

-- This function finds the views of a function and adds them to a set
addViews' :: [Eval] -> Int -> TNode -> TNode
addViews' [] k node = node
addViews' (x:chans) k node =
  addViews' chans k (S.union node (S.fromList $  views node k x))






