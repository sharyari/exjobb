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

verify :: (CMap, CMap) -> Int -> Bool -> [Char]
verify (t1,t2) k v=
  let result = (verify' (t1,[toConf initial],t2) k True False v) in
  if (result == "True") then
    verify (t1,t2) (k+1) v
  else
    result
   
-- This is the verifier, and it basically iterates alpha $ step $ gamma
-- It is divided into two almost identical functions:
-- If gamma gets false, it does the cheaper operation of only stepping (b=False)
-- If gamma gets true, it creates the longer words (b=True). As long as possible, only step
verify' :: (CMap, [C], CMap) -> Int -> Bool -> Bool -> Bool -> [Char]
verify' args k fst b verbose =
  let
    (newTrie, newConf, newSeen) = alpha k $! step k b $! gamma k b args
    isSafe = L.filter (isBadConfiguration bad) $ newConf
    isSame = L.null newConf
  in
   if (L.length isSafe == 0) then
     if isSame && b then
       "Safe"
     else
       verify' (newTrie, newConf, newSeen) k (fst && not isSame) isSame verbose
   else
     if (fst) then
       if verbose then
         traceShow (traceBad newTrie initial (head isSafe)) "Unsafe"
         else
       "Unsafe"
     else
       "True"

       
