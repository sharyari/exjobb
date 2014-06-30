module Alpha where
import Data.Trie as T
import Data.HashSet as S

import DataTypes
import TrieModule
import StringManipulation

-- Alpha takes a list of configurations and a trie, and adds all the views of the configurations in the trie


-- This is a function used for merging the trees. For some reason, it must be of type maybe
myFunc a b = Just (S.union a b)


-- This function creates an empty trie, adds all new configurations to the trie and then
-- merges the two tries to a new one.
--alpha (trie, seen, list) k = ((T.mergeBy (myFunc) trie $ alpha' (T.empty, list) k), seen)

alpha (trie, seen, list) k = (alphA trie list k, seen)

-- This function is here due to prevent stack space overflow. Without this function, the depth of alpha
-- would equal the number of configurations that are to be added, which may be quite a few. This puts a cap
-- on 10000.
alphA trie list k =
  if length list > 100 then
    T.mergeBy (myFunc) (alpha' (T.empty, take 100000 list) k) (alphA trie (drop 10000 list) k)
  else
    (T.mergeBy (myFunc) trie $ alpha' (T.empty, list) k)


-- This is the actual alpha function. It iterates over the configurations and updates the trie with their views
alpha' :: (CTrie,[C]) -> Int -> CTrie
alpha' (trie, []) k = trie
alpha' (trie,(x:xs)) k = alpha' ((addViews trie x k), xs) k

-- This function creates the views of a single configuration, and adds it to the trie
-- THIS FUNCTION SHOULD BE USING Views, not simpleViews. It is likely that simpleViews will be correct - prove it.
addViews :: CTrie -> C -> Int -> CTrie
addViews trie Null k= trie
addViews trie (Conf states chan) k = tAddList trie states (views k chan)
