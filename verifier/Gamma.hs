module Gamma where
import Data.List as L
import DataTypes
import Data.Trie as T
import Data.Set as S
import Data.ByteString

import Alpha
import StringManipulation

-- Gamma takes all configurations, i.e. the trie, and for each configuration:
-- 1. extend the configuration (longer channels)
-- 2. If the extended configuration is in the trie, ignore it
-- 3. else check if all its "simple" views are in the trie (necessarily in the same node)
-- If not, ignore it for now, else, return it

gamma :: CTrie -> Int -> (CTrie,[C])
gamma trie k = let nodes = T.toList trie in
  (trie,L.concat [createConfigurations (fst x) (gamma' (snd x) k) | x <- nodes])


ifNotStepped :: TNode -> [String] -> Bool
ifNotStepped  node strl = if (S.member (strl, False) node) then True else False



-- This looks like it takes a lot of time, but it made everything %30 faster
gamma'' l = S.toList (S.fromList ((L.concat ((L.map longer (L.map fst (S.toList l)))))))

-- This help function creates the longerwords, and removes the ones previously accepted
gamma' :: TNode -> Int -> [[String]]
gamma' stringset k =
  L.filter (ifNotStepped (stringset)) (gamma'' stringset)

-- This function checks if all the subwords of a configuration are in the set
canBeCreated :: Set [String] -> [String] -> Int -> Bool
canBeCreated stringSet string k = ((simpleViews k string) S.\\ stringSet) == S.empty

-- This converts node elements back to configurations
createConfigurations :: ByteString -> [[String]] -> [C]
createConfigurations states [] = []
createConfigurations states (eval:list) = (Conf states eval):createConfigurations states list

