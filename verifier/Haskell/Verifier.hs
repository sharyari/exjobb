module Verifier where

import Data.Word
import Data.List as L
import Debug.Trace
import Data.HashMap.Strict as M
import Data.ByteString as B
import Step
import Gamma
import Alpha
import HashMapModule
import DataTypes
import Run
import UnOrdered
import ProblemFormulation
import Debug.Trace
-- Verify: If a bad configuration is found, increase k
verify :: (CMap, CMap) -> Int -> Bool -> [Char]
verify (t1,t2) k v=
  let result = (verifyNoAbstract [toConf initial] k v) in
  if (result == "BadConf") then
    verify (t1,t2) (k+1) v
  else
    show k ++ " & " ++ result


-- This is a function that converts from CMap2 to CMap
cmap2tocmap :: CMap2 -> CMap
cmap2tocmap trie =
  mapAddList M.empty $ L.map fst $ M.toList trie

-- This function steps configurations, without overapproximation.
-- This means that if a bad configuration is found here, the function is unsafe
-- This function uses CMap2, which differs from CMap in that it keeps the
-- parent of all configurations, in order to create a trace if necessary
-- Note that we are overloading the concept of configuration here
verifyNoAbstract :: [C] -> Int -> Bool -> [Char]
verifyNoAbstract [] k verbose =
  -- If nothing more can be done, start over approximating
  "Empty"
verifyNoAbstract confs k verbose =
  let
    -- the last hashmap together with the new configurations found the new hashmap
    newTrie = run confs k
    -- Check if a bad configuration exists in any of the new configurations
    isSafe = L.filter (isBadConfiguration bad) $ L.map fst $ M.toList newTrie
  in
   if (L.length isSafe == 0) then 
     verifyAbstract (cmap2tocmap newTrie, [], M.empty) k False verbose 
   else -- Bad configuration was found
     if verbose then
       traceBad newTrie (toConf initial) $ L.head isSafe
     else
       "Unsafe"

-- This function overapproximates the configurations. If a bad configuration
-- is found, this indicates that we should increase k
verifyAbstract :: (CMap, [C], CMap) -> Int -> Bool -> Bool -> [Char]
verifyAbstract args k b verbose =
  let
    -- apply gamma, step and alpha in that order, in order to find a set of new configurations
    (newTrie, newConf, newSeen) = alpha k $! step k b $! gamma k b args
    -- Check if the new configurations are sound
    isSafe = L.filter (isBadConfiguration bad) $ newConf
    -- If no new configuratoins are found, set isSame to true
    isSame = L.null newConf
  in
   if (L.length isSafe == 0) then -- is safe
     if isSame && b then -- if we found no new configurations twice in a row, search is done
       traceShow (getSize newTrie) "Safe"
     else
       verifyAbstract (newTrie, newConf, newSeen) k isSame verbose
   else -- bad configuration found
       "BadConf"

       
