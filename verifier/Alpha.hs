module Alpha where
import Data.Trie as T
import Data.Set as S

import DataTypes
import TrieModule
import StringManipulation

-- Alpha takes a list of configurations and a trie, and adds all the views of the configurations in the trie

k=2

alpha :: (Trie (Set ([String], Bool)),[C]) -> Trie (Set ([String], Bool))
alpha (trie,[]) = trie
alpha (trie,(x:xs)) = alpha ((addViews trie x),xs)

addViews :: Trie (Set ([String], Bool)) -> C -> Trie (Set ([String], Bool))
addViews trie (Conf states chan) = tAddList trie ([Conf states x | x <- views k chan]) False