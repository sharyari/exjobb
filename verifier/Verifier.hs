module Verifier where

import Data.Trie as T
import Data.Set as S

import Step
import Gamma
import Alpha
import TrieModule
import DataTypes
import Run
import Data.ByteString as B
import Prelude as P
import Data.Word
import Data.List as L
import Debug.Trace

import System.Environment
import Control.Monad
import Control.Parallel
import Control.Parallel.Strategies


isBad Null = False
isBad (Conf state chan) = (B.last state == (4 :: Word8))

isBad2 (a,b) = B.last a == 4

verify :: (CTrie, CTrie) -> Trie [R] -> [Word8]-> [[ByteString]] -> Int -> CTrie
verify tries rules initial symbols k =
  let
    isSafe =  S.size $ S.filter (isBad) (run (([toConf initial]),[]) rules k)
    result =  (verify' tries rules symbols k 100)
    isSafe2 = L.length $ L.filter (isBad2) $ T.toList result
  in
      isSafe2 `par` isSafe `pseq` if isSafe > 0 then trace "Bad state entered, K= " $ traceShow isSafe T.empty else
    if isSafe2 > 0 then
      trace "Bad configuration found with K=" $ traceShow k $verify tries rules initial symbols (k+1)
    else
      trace "System found to be found safe with K=" $ traceShow k result

-- This is the verifier, and it basically iterates alpha $ step $ gamma
-- It is divided into two almost identical functions:
-- If gamma gets false, it does the cheaper operation of only stepping
-- If gamma gets true, it creates the longer words. As long as possible, only step
verify' :: (CTrie, CTrie) -> Trie [R] -> [[ByteString]] -> Int -> Int -> CTrie
verify' (trie,seen) rules symbols _ 0 = trie
verify' (trie,seen) rules symbols k c =
  let
    nextIteration = alpha (step (gamma (trie,seen) symbols k False ) rules ) k
    isSafe = L.length $ L.filter (isBad2) $ T.toList $ fst nextIteration
   in
  if ((getSize (fst nextIteration)) == getSize trie) then
    verify2 nextIteration rules symbols k (c-1)
  else if isSafe > 0 then
    fst nextIteration
  else
    verify' nextIteration rules symbols k (c-1)

verify2 :: (CTrie, CTrie) -> Trie [R] -> [[ByteString]] -> Int -> Int -> CTrie
verify2 (trie,seen) rules symbols _ 0 = trie
verify2 (trie,seen) rules symbols k c =
  let
    nextIteration = alpha (step (gamma (trie,seen) symbols k True ) rules ) k
    isSafe = L.length $ L.filter (isBad2) $ T.toList $ fst nextIteration
  in
  if ((getSize (fst nextIteration)) == getSize trie) then
    fst nextIteration
  else if isSafe > 0 then
    fst nextIteration
  else
    verify' nextIteration rules symbols k (c-1)
