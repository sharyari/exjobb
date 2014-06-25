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
applyRule Null _ = Null
applyRule (Conf states chan) (Rule newState (i, "_", symbol)) =
  Conf newState chan
applyRule (Conf states chan) (Rule newState (i, "?", symbol)) =
  if (L.length (chan!!i) > 0 && [P.head (chan!!i)] == symbol) then Conf newState (replaceNth i (P.tail (chan!!i)) chan) else Null
applyRule (Conf states chan) (Rule newState (i, "!", symbol)) =
  Conf newState (replaceNth i (chan!!i++symbol) chan)
applyRule (Conf states chan) (Rule newState (i, "¡", symbol)) =
  Conf newState (replaceNth i (symbol++chan!!i) chan)

replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs


step :: (CTrie, [C]) -> Trie [R] -> (CTrie, [C])
step (trie, confs) rules = (trie, L.concat (L.map (applyRules rules) confs))




translate :: ([(Int,Int,Int)], (Int, String, String)) -> [([Word8], [Word8], (Int, String, String))]
translate (ilist,tuple) = let res = L.filter (checkPred ilist) combs in [(toW8 x, toW8 (perform ilist x), tuple) | x <- res]




toW8 :: [Int] -> [Word8]
toW8 [] = []
toW8 (i:l) = fromInteger (toInteger i) : toW8 l

perform :: [(Int, Int, Int)] -> [Int] -> [Int]
perform [] il = il
perform ((a,b,c):tl) il = perform tl (replaceNth a c il)

checkPred :: [(Int, Int, Int)] -> [Int] -> Bool
checkPred [] _ = True
checkPred ((a,b,c):l) il = if ((il!!a) /= b ) then False else checkPred l il


numPrograms = 2
numStates1 = 9
numStates2 = 6
numStates3 = 4

combs = sequence [[1..numStates1],[1..numStates2],[1..numStates3]]




-- Help function to create empty configuration from int-list. Only needed for initial configuration and debugging
toConf :: [Word8] -> C
toConf l = Conf (pack l) ["", ""]

