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
import ProblemFormulation

-- Verify: If a bad configuration is found, increase k
verify :: (CMap, CMap) -> Int -> Bool -> [Char]
verify (t1,t2) k v=
  let result = (verifyNoAbstract (M.empty,[(toConf initial, toConf initial)],t2) k v) in
  if (result == "BadConf") then
    verify (t1,t2) (k+1) v
  else
    result


-- This is a function that converts from CMap2 to CMap
cmap2tocmap :: CMap2 -> CMap
cmap2tocmap trie =
  mapAddList M.empty $ L.map fst $ M.toList trie

-- This function steps configurations, without overapproximation.
-- This means that if a bad configuration is found here, the function is unsafe
-- This function uses CMap2, which differs from CMap in that it keeps the
-- parent of all configurations, in order to create a trace if necessary
verifyNoAbstract :: (CMap2, [(C,C)], CMap) -> Int -> Bool -> [Char]
verifyNoAbstract (trie, [], seen) k verbose =
  -- If nothing more can be done, start over approximating
  verifyAbstract (cmap2tocmap trie, [], seen) k False verbose 
verifyNoAbstract (trie, confs, seen) k verbose =
  let
    newTrie = M.union trie $ M.fromList confs
    newConfs = step2 k $ L.map fst confs
    isSafe = L.filter (isBadConfiguration bad) $ L.map fst confs
  in
   if (L.length isSafe == 0) then
    verifyNoAbstract
    (newTrie, L.filter (\x -> not $ M.member (fst x) newTrie) newConfs, seen) k verbose
   else
     if verbose then
       traceBad newTrie (toConf initial) $ L.head isSafe
     else
       "Unsafe"

-- This function overapproximates the configurations. If a bad configuration
-- is found, this indicates that we should increase k
verifyAbstract :: (CMap, [C], CMap) -> Int -> Bool -> Bool -> [Char]
verifyAbstract args k b verbose =
  let
    (newTrie, newConf, newSeen) = alpha k $! step k b $! gamma k b args
    isSafe = L.filter (isBadConfiguration bad) $ newConf
    isSame = L.null newConf
  in
   if (L.length isSafe == 0) then
     if isSame && b then
       "Safe"
     else
       verifyAbstract (newTrie, newConf, newSeen) k isSame verbose
   else
       "BadConf"

       
