module Verifier where

import Data.Word
import Data.List as L
import Debug.Trace
import Data.HashMap.Strict as M

import Step
import Gamma
import Alpha
import TrieModule
import DataTypes
import Run
import UnOrdered

verify :: (CMap, CMap) -> RuleTrie -> [(Int, Word8)] -> [Word8]-> Symbols -> Int -> CMap
verify (t1,t2) rules bad initial symbols k =
  let
    result1 = run ([toConf initial]) rules k                                   -- Reachability analysis
    isSafe =  L.filter (isBadState bad) $ M.toList result1                          -- Check if any bad states exist
    result2 = verify' (t1,[toConf initial],t2) rules bad symbols k False                             -- Create configurations
    isSafe2 = L.length $ L.filter (isBadConfiguration bad) $ M.toList result2     -- Check if any bad conifugrations exist
  in
    if (L.length isSafe) > 0 then
      trace ("Bad state entered, K = " ++show k) $ traceShow (traceBad result1 (toConf initial) (fst $ L.head $ isSafe)) result2
    else if isSafe2 > 0 then
      trace ("Bad configuration found with K = " ++show k) $ verify (t1,t2) rules bad initial symbols (k+1)
    else
      trace ("System found to be safe with K = "++show k) result2


-- This is the verifier, and it basically iterates alpha $ step $ gamma
-- It is divided into two almost identical functions:
-- If gamma gets false, it does the cheaper operation of only stepping (b=False)
-- If gamma gets true, it creates the longer words (b=True). As long as possible, only step
verify' :: (CMap, [C], CMap) -> RuleTrie -> [(Int,Word8)] -> Symbols -> Int -> Bool -> CMap
verify' (trie,new, seen) rules bad symbols k b =
  let
    (newTrie, newConf, newSeen) = alpha (step (gamma (trie,new,seen) symbols k b) rules k b) k
    isSafe = L.length $ L.filter (isBadConfiguration bad) $ M.toList $ newTrie
   in
  if getSize newTrie == getSize trie && b || isSafe > 0 then
    newTrie
  else
    verify' (newTrie, newConf, newSeen) rules bad symbols k (getSize newTrie == getSize trie)


