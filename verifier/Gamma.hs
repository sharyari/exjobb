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
import Control.Parallel.Strategies
import TrieModule
import Step

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
gamma :: (CTrie, CTrie) -> Int -> (CTrie,CTrie, [C])
gamma (trie, seen) k = let newconfs = newConfs seen (T.toList trie) k in
  (trie,tAdd2 seen newconfs, newconfs)


-- This function creates the new configurations
-- converting back and forth to a set is faster than "nub", seems like it is always like that
newConfs :: CTrie -> [(ByteString, TNode)] -> Int -> [C]
newConfs seen nodes k =
  Set.toList . Set.fromList $ (L.concat [createConfigurations (fst x) (gamma' seen x k) | x <- nodes])


gamma' :: CTrie ->  (ByteString, TNode) -> Int -> [[ByteString]]
gamma' seen (state,stringset) k = let
  l1 = (gamma'' stringset k)
  l2 = fromMaybe S.empty $ T.lookup state seen in
    S.toList $ S.difference (S.fromList l1) l2


gamma'' stringset k = let stringlist = (S.toList stringset) in
  stringlist ++ L.concat (pmap (nlonger stringset k) stringlist)


--nlonger :: [ByteString] -> [([ByteString],Int)]
nlonger stringset k sl= let list = nlonger' (L.length sl-1) sl in
  L.map fst $ L.filter (help' stringset k) list

nlonger' :: Int -> [ByteString] -> [([ByteString],Int)]
nlonger' (-1) sl = []
nlonger' n sl = [(replaceNth n x sl,n) | x <- [B2.concat [y,(sl!!n)] | y<-symbols ]]++nlonger' (n-1) sl

help' stringset k bla = S.member (swords k bla) stringset

swords :: Int -> ([ByteString], Int) -> [ByteString]
swords k (x,n) = let xv = x!!n in
  replaceNth n (B.drop (B.length xv-k) xv) x

-- This converts node elements back to configurations
createConfigurations :: ByteString -> [[ByteString]] -> [C]
createConfigurations states [] = []
createConfigurations states (eval:list) = (Conf states eval):createConfigurations states list

