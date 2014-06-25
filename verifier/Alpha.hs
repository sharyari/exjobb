module Alpha where
import Data.Trie as T
import Data.Set as S

import DataTypes
import TrieModule
import StringManipulation


-- Alpha takes a list of configurations and a trie, and adds all the views of the configurations in the trie


k=2

alpha :: (CTrie,[C]) -> CTrie
alpha (trie,[]) = trie
alpha (trie,(x:xs)) = alpha ((addViews trie x),xs)

addViews :: CTrie -> C -> CTrie
addViews trie Null = trie
--addViews trie (Conf states chan) = tAddList trie ([Conf states x | x <- views k chan]) False
addViews trie (Conf states chan) = tAddList trie states (views k chan) False

