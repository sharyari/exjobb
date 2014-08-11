module Gamma where
import Data.List as L
import Data.Trie as T
import Data.HashSet as S
import qualified Data.ByteString.Char8 as B2
import Data.Set as Set

import StringManipulation
import TrieModule
import DataTypes

import Debug.Trace



-- In general, Gamma takes all configurations, i.e. the trie, and for each configuration:
-- 1. extend the configuration (longer channels)
-- 2. If the extended configuration is in the trie, ignore it
-- 3. else check if all its "simple" views are in the trie (necessarily in the same node)
-- If not, ignore it for now, else, return it



-- This function takes a trie of configurations, another trie that marks the configurations
-- which have already stepped and passes them back, with the second trie updated
-- together with a set of configurations that can be created from that information.
gamma :: (CTrie, [C], CTrie) -> Symbols -> Int -> Bool -> (CTrie,CTrie, [C])
gamma (trie, new, seen) symbols k b =
  let newconfs = if b then newConfs seen (T.toList trie) symbols k b else new in
  if b then
    (trie,tAdd2 seen newconfs, newconfs)
  else
    (trie, seen, newconfs)
-- This function creates the new configurations
-- converting back and forth to a set is faster than "nub", seems like it is always like that
newConfs :: CTrie -> [(State, TNode)] -> Symbols -> Int -> Bool -> [C]
newConfs seen nodes symbols k b =
  (L.concat [L.map (createConfigurations (fst x)) (gamma' seen x symbols k b) | x <- nodes])

-- This converts node elements back to configurations
--createConfigurations :: ByteString -> [[ByteString]] -> [C]
createConfigurations states eval = (states, eval)


gamma' :: CTrie ->  (State, TNode) -> Symbols -> Int -> Bool -> [Eval]
gamma' seen (state,stringset) symbols k b =
    gamma'' stringset (findStateInTrie state seen) symbols k

gamma'' :: TNode -> TNode -> Symbols -> Int -> [Eval]
gamma'' stringset seen symbols k =
     L.concatMap (nlonger stringset seen symbols k) (S.toList stringset)

-- This function is here only to change the order of a and b, which prevents it from being inline
unique a b = not $ S.member b a

--nlonger :: TNode -> [[ByteString]] -> [([ByteString],Int)]
nlonger stringset seen symbols k sl =
  nlonger' seen stringset symbols k (L.length sl-1) sl

--nlonger' :: [[ByteString]] -> Int -> [ByteString] -> [([ByteString],Int)]
nlonger' seen stringset symbols k (-1) sl = []
nlonger' seen stringset symbols k n sl =
  (if (L.length (sl!!n) == k) then (L.filter (unique seen) $ L.filter (help' stringset k n)
  [replaceNth n (y:(sl!!n)) sl  | y <- (symbols!!n) ]) else [])
  ++nlonger' seen stringset symbols k (n-1) sl

-- This is a help function that checks whether the subviews (actually a single one) of a concretization are in the trie
help' stringset k n bla = S.member (replaceNth n (L.take k (bla!!n)) bla) stringset

-- THIS DOESN'T WORK FOR STACKS RIGHT NOW!!
