module Gamma where
import Data.List as L
import DataTypes
import Data.Trie as T
import Data.HashSet as S
import Data.ByteString as B
import qualified Data.ByteString.Char8 as B2
import Data.Set as Set

import Data.Maybe (fromMaybe, fromJust, isJust)

import Alpha
import StringManipulation
import TrieModule
import Step

import Debug.Trace
import Control.Parallel.Strategies



-- Gamma takes all configurations, i.e. the trie, and for each configuration:
-- 1. extend the configuration (longer channels)
-- 2. If the extended configuration is in the trie, ignore it
-- 3. else check if all its "simple" views are in the trie (necessarily in the same node)
-- If not, ignore it for now, else, return it



-- This function takes a trie of configurations, another trie that marks the configurations
-- which have already stepped and passes them back, with the second trie updated
-- together with a set of configurations that can be created from that information.
gamma :: (CTrie, CTrie) -> [[ByteString]] -> Int -> Bool -> (CTrie,CTrie, [C])
gamma (trie, seen) symbols k b = let newconfs = newConfs seen (T.toList trie) symbols k b in
  (trie,tAdd2 seen newconfs, newconfs)


-- This function creates the new configurations
-- converting back and forth to a set is faster than "nub", seems like it is always like that
newConfs :: CTrie -> [(ByteString, TNode)] -> [[ByteString]] -> Int -> Bool -> [C]
newConfs seen nodes symbols k b =
  (L.concat [L.map (createConfigurations (fst x)) (gamma' seen x symbols k b) | x <- nodes])

-- This converts node elements back to configurations
--createConfigurations :: ByteString -> [[ByteString]] -> [C]
createConfigurations states eval = (Conf states eval)


gamma' :: CTrie ->  (ByteString, TNode) -> [[ByteString]] -> Int -> Bool -> [[ByteString]]
gamma' seen (state,stringset) symbols k b = let
  l2 = fromMaybe S.empty $ T.lookup state seen in
  if b then
    S.toList $ gamma'' stringset l2 symbols k
  else
    S.toList $ S.difference stringset l2

canBeCreated :: HashSet [ByteString] -> Int -> [ByteString] -> Bool
canBeCreated stringset k string = (S.difference (simpleViews k string) stringset) == S.empty

gamma'' :: TNode -> TNode -> [[ByteString]]  -> Int -> TNode
gamma'' stringset seen symbols k =
    S.unions $ L.map (S.fromList . nlonger stringset seen symbols k) (S.toList stringset)

bla a b = not $ S.member b a

--nlonger :: TNode -> [[ByteString]] -> [([ByteString],Int)]
nlonger stringset seen symbols k sl =
  nlonger' seen stringset symbols k (L.length sl-1) sl 

--nlonger' :: [[ByteString]] -> Int -> [ByteString] -> [([ByteString],Int)]
nlonger' seen stringset symbols k (-1) sl = []
nlonger' seen stringset symbols k n sl =
  L.filter (bla seen) $ L.filter (help' stringset k n)
  [replaceNth n x sl | x <- [B2.concat [y,(sl!!n)] | y<- (symbols!!n) ]]
  ++nlonger' seen stringset symbols k (n-1) sl

help' stringset k n bla = S.member (replaceNth n (B.take k (bla!!n)) bla) stringset

