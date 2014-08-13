module Alpha where
import Data.HashSet as S
import DataTypes
import TrieModule
import StringManipulation
import UnOrdered
import Data.List as L


-- This is alpha. Alpha just calls alpha'. All this function does is to abstract internal
-- ugliness from the outside world.
alpha :: (CMap, CMap, [C]) -> Int -> (CMap,[C],CMap)
alpha (confs, seen, list) k =
  alpha' list k (confs,[],seen)

-- This is the actual alpha function. It will look at the list of new configuration, and take
-- out all with the same state, and the send them to the function addViews which will calculate
-- their views and add them to a set. Then, alpha' merges that with the old set.
alpha' :: [C] -> Int -> (CMap, [C], CMap) -> (CMap,[C],CMap)
alpha' [] k (confs,new,seen) = (confs,new,seen)
alpha' (x:xs) k (confs,new,seen) =
  let
    state = fst x
    (relevant',irrelevant) = L.partition (\y -> (fst y) == (fst x)) (x:xs)
    (newConfs, new') = addViews' (L.map snd relevant') k (findNodeInMap state confs, [])
  in
    alpha' irrelevant k
    (mapAddNode confs state newConfs, L.zip (takeRepeat state) new' ++ new, seen) -- This line would fail if there would be more than 1M equal-state

-- This function finds the views of a Configuration and adds them to a set
addViews' :: [Eval] -> Int -> (MapNode,[Eval]) -> (MapNode,[Eval])
addViews' [] k (node,new) = (node, new)
addViews' (x:chans) k (node,new) =
  let newViews = views node k x in
  addViews' chans k (S.union node $ S.fromList $ newViews, newViews++new)






