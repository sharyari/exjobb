module Alpha where
import Data.Trie as T
import Data.HashSet as S

import DataTypes
import TrieModule
import StringManipulation

-- Alpha takes a list of configurations and a trie, and adds all the views of the configurations in the trie


-- This is a function used for merging the trees. For some reason, it must be of type maybe
myFunc a b = Just (S.union a b)

-- Idea: add all views of equal-state configurations at once
-- THIS IS WHAT IS TAKING ALL THE STACK SPACE PROBABLY

-- This function creates an empty trie, adds all new configurations to the trie and then
-- merges the two tries to a new one.
alpha (trie, seen, list) k = ((T.mergeBy (myFunc) trie $ alpha' (T.empty, list) k), seen)


-- This is the actual alpha function, that creates the views of configurations
alpha' :: (CTrie,[C]) -> Int -> CTrie
alpha' (trie, []) k = trie
alpha' (trie,(x:xs)) k = alpha' ((addViews trie x k), xs) k

addViews :: CTrie -> C -> Int -> CTrie
addViews trie Null k= trie
--addViews trie (Conf states chan) = tAddList trie ([Conf states x | x <- views k chan]) False
addViews trie (Conf states chan) k = tAddList trie states (views k chan)


