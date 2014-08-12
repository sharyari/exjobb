module Verifier where

import Data.Trie as T
import Data.Set as S
import Prelude as P
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

import Control.Parallel
import Control.Parallel.Strategies

-- These functions should be removed and automatized, the include will then be unnecessary
import Data.ByteString as B

isBadState bad (state, chan) = or [index state x == y | (x,y) <- bad]
isBadConfiguration bad (state,eval) = or [index state x == y | (x,y) <- bad]

verify :: (CTrie, CTrie) -> Trie [R] -> [(Int, Word8)] -> [Word8]-> Symbols -> Int -> CTrie
verify (t1,t2) rules bad initial symbols k =
  let
    result1 = run ([toConf initial],[]) rules k                                   -- Reachability analysis
    isSafe =  S.size $ S.filter (isBadState bad) result1                          -- Check if any bad states exist
    result2 = verify' (t1,[toConf initial],t2) rules bad symbols k False                             -- Create configurations
    isSafe2 = L.length $ L.filter (isBadConfiguration bad) $ M.toList result2     -- Check if any bad conifugrations exist
  in
    isSafe2 `par` isSafe `pseq`
    if isSafe > 0 then
      trace "Bad state entered, K= " $ traceShow k $ traceShow "" M.empty
    else if isSafe2 > 0 then
      trace "Bad configuration found with K=" $ traceShow k $ verify (t1,t2) rules bad initial symbols (k+1)
    else
      trace "System found to be safe with K=" $ traceShow k result2



-- This is the verifier, and it basically iterates alpha $ step $ gamma
-- It is divided into two almost identical functions:
-- If gamma gets false, it does the cheaper operation of only stepping (b=False)
-- If gamma gets true, it creates the longer words (b=True). As long as possible, only step
verify' :: (CTrie, [C], CTrie) -> Trie [R] -> [(Int,Word8)] -> Symbols -> Int -> Bool -> CTrie
verify' (trie,new, seen) rules bad symbols k b =
  let
    (newTrie, newConf, newSeen) = alpha (step (gamma (trie,new,seen) symbols k b) rules k b) k
    isSafe = L.length $ L.filter (isBadConfiguration bad) $ M.toList $ newTrie
   in
  if getSize newTrie == getSize trie && b || isSafe > 0 then
    newTrie
  else
    verify' (newTrie, newConf, newSeen) rules bad symbols k (getSize newTrie == getSize trie)


