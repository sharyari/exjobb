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

verify :: (CMap, CMap) ->  Int -> CMap
verify (t1,t2)  k =
  if (verify' (t1,[toConf initial],t2) k True False) then
    verify (t1,t2) (k+1)
  else
    M.empty
   
-- This is the verifier, and it basically iterates alpha $ step $ gamma
-- It is divided into two almost identical functions:
-- If gamma gets false, it does the cheaper operation of only stepping (b=False)
-- If gamma gets true, it creates the longer words (b=True). As long as possible, only step
verify' :: (CMap, [C], CMap) -> Int -> Bool -> Bool -> Bool
verify' args k fst b =
  let
    (newTrie, newConf, newSeen) = alpha k $! step k b $! gamma k b args
    isSafe = L.filter (isBadConfiguration bad) $ newConf
    isSame = L.null newConf
  in
   if (L.length isSafe == 0) then
     if isSame && b then
       traceShow "System is safe" $ traceShow (getSize newTrie) False
     else
       verify' (newTrie, newConf, newSeen) k (fst && not isSame) isSame
   else
     if (fst) then
       traceShow "Bad State found:  \n" False
     else
       traceShow "Bad configuration found" True

       
