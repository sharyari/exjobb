module Step where
import Data.Trie as T
import Data.Set as S
import Prelude as P

import DataTypes
import TrieModule
import UnOrdered

-- This function applies rules on a list of concretizations to create new configurations
step :: (CMap, CMap, [C]) -> RuleTrie ->  Int -> Bool -> (CMap, CMap, [C])
step (confs, seen, newConfs) rules k b=
  (confs, seen, (concatMap (applyRules rules seen confs k b ) newConfs))

-- This function takes a concretization and applies all relevant rules upon it
-- it then concatenates the result
applyRules :: RuleTrie -> CMap -> CMap -> Int -> Bool -> C -> [C]
applyRules rules seen trie k b (state, chan) =
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



