module Step where

import Data.Set as S
import Prelude as P

import DataTypes
import TrieModule
import UnOrdered
import Debug.Trace


-- This function applies rules on a list of concretizations to create new configurations
step :: Int -> Bool -> (CMap, CMap, [C]) ->(CMap, CMap, [C])
step k b (confs, seen, newConfs)=
  (confs, seen, (concatMap (applyRules seen confs k b ) newConfs))

-- This function takes a concretization and applies all relevant rules upon it
-- it then concatenates the result
applyRules :: CMap -> CMap -> Int -> Bool -> C -> [C]
applyRules seen trie k b (state, chan) =
  if b
  then
    P.filter (ifSeen seen) $ P.filter (ifSeen trie) $ concatMap (applyRule chan k) $ findNodeInTrie state rules
  else
    P.filter (ifSeen trie) $ concatMap (applyRule chan k) $ findNodeInTrie state rules

-- This function applies a single rule to a single concretization
applyRule ::  Eval -> Int -> Rule -> [C]
applyRule chan k (newState, (i, tr, symbol))
  | tr == "_" =
    [(newState, chan)]
  | tr == "?" =
    if length (chan!!i) > 0 && [last (chan!!i)] == symbol then
      [(newState, replaceNth i (init (chan!!i))  chan)] else []
  | tr == "¡" =
    let
      w = chan!!i++symbol
      newWord1 = reverse $ take k $ reverse $ w
      newWord2 = reverse $ take k $ drop 1 $ reverse  $ w
    in
     if (length w > k) then
       [(newState, replaceNth i newWord1 chan),(newState,replaceNth i newWord2 chan)]
     else
       [(newState, replaceNth i w chan)]
  | tr == "!" =
    let
      w = symbol++chan!!i
      newWord1 = reverse $ take k $ reverse $ w
      newWord2 = reverse $ take k $ drop 1 $ reverse  $ w
    in
      if (length w > k) then
        [(newState, replaceNth i newWord1 chan),(newState, replaceNth i newWord2 chan)]
      else
        [(newState, replaceNth i w chan)]



step2 :: Int ->  [C] -> [(C,C)]
step2 k [] = []
step2 k ((state, chan):xs) =
  concatMap (applyRule2 state chan k) $ findNodeInTrie state rules


applyRule2 :: State -> Eval -> Int -> Rule -> [(C,C)]
applyRule2 state chan k (newState, (i, tr, symbol))
  | tr == "_" =
    [((newState, chan), (state, chan))]
  | tr == "?" =
    if length (chan!!i) > 0 && [last (chan!!i)] == symbol then
      [((newState, replaceNth i (init (chan!!i)) chan), (state, chan))] else []
  | tr == "¡" =
    let
      w = chan!!i++symbol
      newWord1 = reverse $ take k $ reverse $ w
      newWord2 = reverse $ take k $ drop 1 $ reverse  $ w
    in
     if (length w > k) then
       [((newState, replaceNth i newWord1 chan),(state, chan)),((newState,replaceNth i newWord2 chan), (state, chan))]
     else
       [((newState, replaceNth i w chan), (state, chan))]
  | tr == "!" =
    let
      w = symbol++chan!!i
      newWord1 = reverse $ take k $ reverse $ w
      newWord2 = reverse $ take k $ drop 1 $ reverse  $ w
    in
      if (length w > k) then
        [((newState, replaceNth i newWord1 chan), (state, chan)),((newState, replaceNth i newWord2 chan),(state, chan))]
      else
        [((newState, replaceNth i w chan), (state, chan))]
