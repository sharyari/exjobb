module Alpha where

import Data.HashSet as S
import Data.List as L
import Data.HashMap.Strict as M

import DataTypes
import HashMapModule
import StringManipulation
import Data.Ord -- comparing

-- Given a set of new configurations, alpha adds those configurations and their views
-- to the set of known configurations
alpha :: Int -> (CMap, CMap, [C]) ->  (CMap,[C],CMap)
alpha k (confs, seen, list) =
    let new = alpha' (sortBy (comparing fst) list) k in
    (M.unionWith (S.union) confs (M.fromList new), L.concat $ L.map help new, seen)

-- This function converts a state and a list of evaluations to a list of state-evaluation pairs
help (a,b) = [(a,x) | x <- S.toList b]

-- This function takes a list of new configuration, and takes out all configurations
-- with the same state. It then sends them to the function addViews which will calculate
-- their views and add them to a set. Then, alpha' merges the result with the old set.
-- Note that this requires the list to be sorted
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






