module Gamma where
import Data.List as L
import Data.HashSet as S
import qualified Data.ByteString.Char8 as B2
import Data.HashMap.Strict as M

import StringManipulation
import HashMapModule
import DataTypes
import UnOrdered
import ProblemFormulation

-- In general, Gamma takes all configurations, i.e. the trie, and for each configuration:
-- 1. extend the configuration (longer channels)
-- 2. If the extended configuration is in the trie, ignore it
-- 3. else check if all its "simple" views are in the trie (necessarily in the same node)
-- If not, ignore it for now, else, return it


-- This function takes a hashmap of configurations, another hashmap that marks the configurations
-- which have already stepped and passes them back, with the second trie updated
-- together with a set of configurations that can be created from that information.
gamma :: Int -> Bool -> (CMap, [C], CMap) -> (CMap,CMap, [C])
gamma k b (hmap, new, seen)=
  let newconfs = if b then newConfs seen (M.toList hmap) k else new in
  if b then
    (hmap,mapAddList seen newconfs, newconfs)
  else
    (hmap, seen, newconfs)

-- This function creates the new concretizations
newConfs :: CMap -> [(State, MapNode)] -> Int -> [C]
newConfs seen nodes k =
  (L.concat [L.zip (takeRepeat $ fst x) (newConfs' (snd x) (findNodeInMap (fst x) seen) k) | x <- nodes])

-- This function creates concretizations from a configuration
newConfs' :: MapNode -> MapNode -> Int -> [Eval]
newConfs' stringset seen k =
     L.concatMap (nlonger stringset seen k) (S.toList stringset)

-- This function creates longer evaluations from an evaluation. The work is done
-- by nlonger', this function only adds the number of evaluations as a parameter
nlonger :: MapNode -> MapNode -> Int -> Eval -> [Eval]
nlonger stringset seen k sl =
  nlonger' seen stringset k (L.length sl-1) sl

-- This function creates longer evaluations from an evaluation. Only those evaluations
-- which have a single symbol added to the end are considered.
nlonger' :: MapNode -> MapNode -> Int -> Int -> Eval -> [Eval]
nlonger' seen stringset k (-1) sl = []
nlonger' seen stringset k n sl =
  ((if (L.length (sl!!n) == k) then (L.filter (\x -> not $ S.member x seen)
  $! L.filter ((\x -> S.member (replaceNth n (L.take k (x!!n)) x) stringset))
  [replaceNth n (y:(sl!!n)) sl  | y <- (symbols!!n) ]) else []))
  ++nlonger' seen stringset k (n-1) sl



