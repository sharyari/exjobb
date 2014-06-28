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

import Control.Parallel.Strategies
pmap f x = (L.map f x) `using` parList rdeepseq
pfilter f x = (L.filter f x) `using` parList rdeepseq


-- Gamma takes all configurations, i.e. the trie, and for each configuration:
-- 1. extend the configuration (longer channels)
-- 2. If the extended configuration is in the trie, ignore it
-- 3. else check if all its "simple" views are in the trie (necessarily in the same node)
-- If not, ignore it for now, else, return it



-- This function takes a trie of configurations, another trie that marks the configurations
-- which have already stepped and passes them back, with the second trie updated
-- together with a set of configurations that can be created from that information.
gamma :: (CTrie, CTrie) -> Int -> Bool -> (CTrie,CTrie, [C])
gamma (trie, seen) k b = let newconfs = newConfs seen (T.toList trie) k b in
  (trie,tAdd2 seen newconfs, newconfs)


-- This function creates the new configurations
-- converting back and forth to a set is faster than "nub", seems like it is always like that
newConfs :: CTrie -> [(ByteString, TNode)] -> Int -> Bool -> [C]
newConfs seen nodes k b =
  Set.toList . Set.fromList $ (L.concat [createConfigurations (fst x) (gamma' seen x k b) | x <- nodes])

-- This converts node elements back to configurations
createConfigurations :: ByteString -> [[ByteString]] -> [C]
createConfigurations states [] = []
createConfigurations states (eval:list) = (Conf states eval):createConfigurations states list


gamma' :: CTrie ->  (ByteString, TNode) -> Int -> Bool -> [[ByteString]]
gamma' seen (state,stringset) k b = let
  l1 = if b then gamma'' stringset k else stringset
  l2 = fromMaybe S.empty $ T.lookup state seen in
   S.toList $ S.difference l1 l2

canBeCreated :: HashSet [ByteString] -> Int -> [ByteString] -> Bool
canBeCreated stringset k string = (S.difference (simpleViews k string) stringset) == S.empty

gamma'' :: TNode -> Int -> TNode
gamma'' stringset k =
  let stringlist = S.toList stringset in
  S.fromList $  L.concat (pmap (nlonger stringset k) stringlist)

--------------------------------------------------------
--------------------- OBSELETE -------------------------
----longer gives more configurations. It runs slower,---
----but proportionally so, considering the number of----
----extra configurations found. ------------------------
--------------------------------------------------------
--nlonger :: [ByteString] -> [([ByteString],Int)]
nlonger stringset k sl= let list = nlonger' (L.length sl-1) sl in
  L.map fst $ L.filter (help' stringset k) list

nlonger' :: Int -> [ByteString] -> [([ByteString],Int)]
nlonger' (-1) sl = []
nlonger' n sl = [(replaceNth n x sl,n) | x <- [B2.concat [y,(sl!!n)] | y<- symbols ]]++nlonger' (n-1) sl

help' stringset k bla = S.member (swords k bla) stringset

swords :: Int -> ([ByteString], Int) -> [ByteString]
swords k (x,n) = let xv = x!!n in
  if (B.length xv < k) then x else
    replaceNth n (B.take k xv) x


