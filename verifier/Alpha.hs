module Alpha where
import Data.Trie as T
import Data.Set as S

import DataTypes
import TrieModule
import StringManipulation

-- Alpha takes a list of configurations and a trie, and adds all the views of the configurations in the trie


-- Idea: add all views of equal-state configurations at once
alpha :: (CTrie,CTrie, [C]) -> Int -> (CTrie, CTrie)
alpha (trie,seen, []) k = (trie, seen)
alpha (trie,seen,(x:xs)) k = alpha ((addViews trie x k),seen, xs) k

addViews :: CTrie -> C -> Int -> CTrie
addViews trie Null k= trie
--addViews trie (Conf states chan) = tAddList trie ([Conf states x | x <- views k chan]) False
addViews trie (Conf states chan) k = tAddList trie states (views k chan)


