module Gamma where
import Data.List as L
import DataTypes
import Data.Trie as T
import Data.Set as S
import Data.ByteString as B
import qualified Data.ByteString.Char8 as B2


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
-- converting back and forth to a set is faster than "nub" in this case
-- Likely due to the set being ordered, putting the conversion in a best case scenario each time
newConfs :: CTrie -> [(ByteString, TNode)] -> Int -> [C]
newConfs seen nodes k =
  S.toList (S.fromList (L.concat [createConfigurations (fst x) (gamma' seen x k) | x <- nodes]))

-- This is a filter function that removes the candidates which have been considered in a previous
-- iteration of gamma
alreadySeen :: TNode -> [ByteString] -> Bool
alreadySeen node element = S.notMember element node


gamma' :: CTrie ->  (ByteString, TNode) -> Int -> [[ByteString]]
gamma' seen (state,stringset) k = pfilter (alreadySeen (fromMaybe S.empty (T.lookup state seen))) (gamma'' stringset k)

gamma'' stringset k = L.concat (pmap (nlonger stringset k) (S.toList stringset))


--nlonger :: [ByteString] -> [([ByteString],Int)]
nlonger stringset k sl= let list = nlonger' (L.length sl) sl in
  L.map fst (L.filter (help' stringset k) list)

nlonger' :: Int -> [ByteString] -> [([ByteString],Int)]
nlonger' 0 sl = []
nlonger' n sl= [(replaceNth (n-1) x sl,(n-1)) | x <- [B2.concat [(sl!!(n-1)),y] | y<-symbols ]]++nlonger' (n-1) sl

help' stringset k bla = S.member (swords k bla) stringset

swords :: Int -> ([ByteString], Int) -> [ByteString]
swords k (x,n) = replaceNth n (B.drop (B.length (x!!n)-k) (x!!n)) x

-- This converts node elements back to configurations
createConfigurations :: ByteString -> [[ByteString]] -> [C]
createConfigurations states [] = []
createConfigurations states (eval:list) = (Conf states eval):createConfigurations states list

