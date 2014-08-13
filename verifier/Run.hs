module Run where
import DataTypes
import UnOrdered

import Prelude as P
import Data.Trie as T
import Data.HashMap.Strict as S
import qualified Data.ByteString.Char8 as B2
import Data.Maybe (fromMaybe, fromJust, isJust)

-- This function will step configurations until no new ones are created any more.
run :: [C] -> RuleTrie -> Int -> HashMap C C
run list rules k = run' (S.fromList (P.zip list (takeRepeat (B2.empty,[]))),S.empty) rules k

-- This function takes a list of configurations, and takes a step. It then calls itself with
-- the newfound configurations as input. If no configurations are found, it stops and returns
-- a hashmap containing all found configurations.
run' :: (HashMap C C, HashMap C C) -> RuleTrie -> Int -> HashMap C C
run' (new, old) rules k
  | new == S.empty = old
  | otherwise  =
  let
    new' = S.difference (S.fromList $ P.concatMap (applyRules rules k) $ S.toList new) old
  in
    run' (new', S.union old new') rules k


applyRules :: RuleTrie -> Int -> (C,C) -> [(C,C)]
applyRules trie k ((states, chan), parent) =
  P.concatMap (applyRule states chan k) (fromMaybe [] $ T.lookup states trie)


applyRule :: State -> Eval -> Int -> Rule -> [(C,C)]
applyRule states chan k (newState, (i, tr, symbol))
  | tr == "_" =
    [((newState, chan), (states, chan))]
  | tr == "?" && P.length (chan!!i) > 0 && [P.last (chan!!i)] == symbol =
    [((newState, (replaceNth i (P.init (chan!!i))  chan)) ,(states, chan))]
  | tr == "¡" =
    [((newState, (replaceNth i (P.take k $ chan!!i++symbol) chan)), (states, chan))]
  | tr == "!" =
    [((newState, (replaceNth i (P.take k $ symbol++chan!!i) chan)), (states, chan))]
  | otherwise = []

