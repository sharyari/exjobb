module Step where

import Data.Set as S
import Prelude as P

import DataTypes
import HashMapModule
import UnOrdered


-- This function applies rules on a list of concretizations to create new configurations
step :: Int -> Bool -> (CMap, CMap, [C]) ->(CMap, CMap, [C])
step k b (confs, seen, newConfs)=
  (confs, seen,(P.filter (ifSeen confs) $ P.filter (ifSeen seen) newConfs) ++ (concatMap (applyRules seen confs k b ) newConfs))

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
  | tr == "_" = -- Rule has no channel-predicate
    [(newState, chan)]
  | tr == "?" = -- Rule is a read-operation
    if length (chan!!i) > 0 && [last (chan!!i)] == symbol then
      [(newState, replaceNth i (init (chan!!i))  chan)] else []
  | tr == "¡" = -- Rule is an append-opration
    let
      w = (chan!!i)++symbol
      newWord1 = reverse $ take k $ reverse $ w
      newWord2 = reverse $ take k $ drop 1 $ reverse  $ w
    in
     -- If the produced word is larger than k, create the two k-sized words of the first (k+1) symbols
      if (length w > k) then
        [(newState, replaceNth i newWord1 chan),(newState, replaceNth i newWord2 chan)]
      else
        [(newState, replaceNth i w chan)]
  | tr == "!" = -- rule is a write-operation
    let
      w = symbol++chan!!i 
      newWord1 = reverse $ take k $ reverse $ w
      newWord2 = reverse $ take k $ drop 1 $ reverse  $ w
    in
     -- If the produced word is larger than k, create the two k-sized words of the first (k+1) symbols
      if (length w > k) then
        [(newState, replaceNth i newWord1 chan),(newState, replaceNth i newWord2 chan)]
      else
        [(newState, replaceNth i w chan)]


-- This function is analogous to the function step above, but creates configuration-parent
-- pairs, which may be used to compute a minimal trace
step2 :: Int ->  [C] -> [(C,C)]
step2 k [] = []
step2 k ((state, chan):xs) =
  (P.map (\x -> (x, (state, chan))) $ concatMap (applyRule chan k) $ findNodeInTrie state rules)++step2 k xs
