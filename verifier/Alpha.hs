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
  let (newTrie, newConfs) = (alpha' (trie,[])  list k) in
  (newTrie, newConfs, seen)



-- This function checks if a configuration has a certain state
hasState s (state, chan) = (s == state)
-- Get functions, returns the evaluation of a configuration
getEval (state, chan) = chan

-- This is the actual alpha function. It iterates over the configurations and updates the trie with their views
alpha' :: (CTrie, [C]) -> [C] -> Int -> (CTrie,[C])
alpha' (trie,new) [] k = (trie,new)
alpha' (trie,new) ((state, chan):xs) k=
  let
    (relevant',irrelevant) = L.partition (hasState state) ((state, chan):xs)
    (ntrie, new') = addViews' (L.map getEval relevant') k (findStateInTrie state trie, [])
  in
    alpha' (tChange trie state ntrie, (L.map (createConfigurations state) new')++new) irrelevant k

createConfigurations states eval = (states, eval)



-- This function finds the views of a function and adds them to a set
addViews' :: [Eval] -> Int -> (TNode,[Eval]) -> (TNode,[Eval])
addViews' [] k (node,new) = (node, new)
addViews' (x:chans) k (node,n) =
  let new = views node k x in
  addViews' chans k (S.union node $ S.fromList $ new, new++n)






