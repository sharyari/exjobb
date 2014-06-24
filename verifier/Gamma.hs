module Gamma where
import Data.List as L
import DataTypes
import Data.Trie as T
import Data.Set as S
import Data.ByteString

import StringManipulation

-- Gamma takes all configurations, i.e. the trie, and for each configuration:
-- 1. extend the configuration (longer channels)
-- 2. If the extended configuration is in the trie, ignore it
-- 3. else check if all its "simple" views are in the trie (necessarily in the same node)
-- If not, ignore it for now, else, return it

gamma :: Trie (Set ([String], Bool)) -> (Trie (Set ([String], Bool)),[C])
gamma trie = let nodes = T.toList trie in
  (trie,L.concat [createConfigurations (fst x) (gamma' (snd x)) | x <- nodes])


ifNotStepped :: [([String],Bool)] -> [String] -> Bool
ifNotStepped [] _ = False
ifNotStepped  ((str, bln):list) strl = if (str == strl) then (not bln) else ifNotStepped list strl


-- This help function creates the longerwords, and removes the ones previously accepted
gamma' :: Set ([String], Bool) -> [[String]]
gamma' stringset =
  S.toList(S.filter (ifNotStepped (S.toList stringset)) (S.unions (S.toList (S.map longer (S.map fst stringset)))))
--  S.toList (S.map fst (S.filter (ifNotStepped) p))
--gamma' stringset = L.filter (canBeCreated (stringset) (S.toList (S.unions (S.toList (S.map longer stringset)) S.\\ stringset))

-- This function checks if all the subwords of a configuration are in the set
canBeCreated :: Set [String] -> [String] -> Bool
canBeCreated stringSet string = ((simpleViews k string) S.\\ stringSet) == S.empty

k=2

createConfigurations :: ByteString -> [[String]] -> [C]
createConfigurations states [] = []
createConfigurations states (eval:list) = (Conf states eval):createConfigurations states list

