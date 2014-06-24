module Step where
import Data.Trie as T
import Data.ByteString as B
import Data.List as L
import Data.Set as S
import Prelude as P
import Data.Word
import Data.Maybe (fromMaybe, fromJust, isJust)


import DataTypes
import TrieModule

createRuleTree :: [([Word8], [Word8], (Int, String, String))] -> Trie [R]
createRuleTree [] = T.empty
createRuleTree ((w1,w2,tuple):xs) = tAddRule (createRuleTree xs) ((pack w1,pack w2, tuple))


applyRules :: Trie [R] -> C -> [C]
applyRules trie (Conf states chan) = let s = T.lookup states trie in
  if (isJust s) then
    [applyRule (Conf states chan) x | x <- (fromJust s)]
  else []

applyRule :: C -> R -> C
applyRule (Conf states chan) (Rule newState (i, "-", symbol)) =
  Conf newState chan
applyRule (Conf states chan) (Rule newState (i, "?", symbol)) =
  if ([P.head (chan!!i)] == symbol) then Conf newState (replaceNth i (P.tail (chan!!i)) chan) else Null
applyRule (Conf states chan) (Rule newState (i, "!", symbol)) =
  Conf newState (replaceNth i (chan!!i++symbol) chan)
applyRule (Conf states chan) (Rule newState (i, "¡", symbol)) =
  Conf newState (replaceNth i (symbol++chan!!i) chan)


replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs


step :: (Trie (Set ([String], Bool)), [C]) -> (Trie (Set ([String], Bool)), [C])
step (trie, confs) = (trie, L.concat (L.map (applyRules rules) confs))



rule1 = ([1,1], [1, 2], (1, "-","s"))
rule2 = ([1,1], [1,1],  (1, "!","a"))

rules = createRuleTree [rule1,rule2]