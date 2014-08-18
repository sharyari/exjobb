module Alpha where
import Data.HashSet as S
import DataTypes
import TrieModule
import StringManipulation
import UnOrdered
import Data.List as L
import Data.Ord
import Data.HashMap.Strict as M

-- This is alpha. Alpha just calls alpha'. All this function does is to abstract internal
-- ugliness from the outside world.
alpha :: (CMap, CMap, [C]) -> Int -> (CMap,[C],CMap)
alpha (confs, seen, list) k =
  let new = alpha' (sortBy (comparing fst) list) k in
  (M.unionWith (S.union) confs (M.fromList new), L.concat $ L.map help new, seen)

help (a,b) = [(a,x) | x <- S.toList b]

-- This is the actual alpha function. It will look at the list of new configuration, and take
-- out all with the same state, and the send them to the function addViews which will calculate
-- their views and add them to a set. Then, alpha' merges that with the old set.
alpha' :: [C] -> Int -> [(State,MapNode)]
alpha' [] k = []
alpha' xs k =
  let
    state = (fst $ head xs)
  in
    (state, addViews' (L.map snd $ L.takeWhile (\y -> (fst y) == state) xs) k S.empty):
    (alpha' (L.dropWhile (\y -> (fst y) == state) xs) k)
    

-- This function finds the views of a Configuration and adds them to a set
addViews' :: [Eval] -> Int -> MapNode -> MapNode
addViews' [] k node = node
addViews' (x:chans) k node =
    addViews' chans k (S.union node $ S.fromList $ views node k x)






