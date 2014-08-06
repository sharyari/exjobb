module Step where
import Data.Trie as T
import Data.ByteString as B
import Data.List as L
import Data.Set as S
import Prelude as P
import Data.Word
import qualified Data.ByteString.Char8 as B2
import Data.Maybe (fromMaybe, fromJust, isJust)

import DataTypes
import TrieModule
import ProblemFormulation
import StringManipulation


-- This is the main function of the file, it will apply appropriate rules to configurations
step :: (CTrie, CTrie, [C]) -> Trie [R] ->  Int -> (CTrie, CTrie, [C])
step (trie, seen, confs) rules k=
  (trie, seen, (L.concat $ P.map (applyRules rules k ) confs))


applyRules :: Trie [R] -> Int -> C -> [C]
applyRules rules k Null = [Null]
applyRules trie k (Conf states chan) =
  let s = fromMaybe [] $ T.lookup states trie in
  L.concat $ L.map (applyRule states (P.map B2.unpack chan) k) s

--applyRule :: C -> R -> C
applyRule states chan k (Rule newState (i, "_", symbol))=
  [Conf newState (P.map B2.pack chan)]
applyRule states chan k (Rule newState (i, "?", symbol)) =
  if (P.length (chan!!i) > 0 && [P.last (chan!!i)] == symbol) then
  [Conf newState (P.map B2.pack (replaceNth i (P.init (chan!!i))  chan))] else [Null]
applyRule states chan k (Rule newState (i, "¡", symbol))=
  let
    w = chan!!i++symbol
    newWord1 = P.reverse $ P.take k $ P.reverse $ w
    newWord2 = P.reverse $ P.take k $ P.drop 1 $ P.reverse  $ w
  in
   if (P.length w > k) then
     [Conf newState $P.map B2.pack $ replaceNth i newWord1 chan,Conf newState $ P.map B2.pack $ replaceNth i newWord2 chan]
   else
     [Conf newState $P.map B2.pack $ replaceNth i w chan]

applyRule states chan k (Rule newState (i, "!", symbol))=
  let
    w = symbol++chan!!i
    newWord1 = P.reverse $ P.take k $ P.reverse $ w
    newWord2 = P.reverse $ P.take k $ P.drop 1 $ P.reverse  $ w
  in
   if (P.length w > k) then
     [Conf newState $P.map B2.pack $ replaceNth i newWord1 chan,Conf newState $ P.map B2.pack $ replaceNth i newWord2 chan]
   else
     [Conf newState $P.map B2.pack $ replaceNth i w chan]

--- This is used once in the beginning, to generate a tree of rules
createRuleTree :: [([Word8], [Word8], (Int, String, String))] -> Trie [R]
createRuleTree [] = T.empty
createRuleTree ((w1,w2,tuple):xs) = tAddRule (createRuleTree xs) ((pack w1,pack w2, tuple))

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

combs :: [[Int]]
combs = sequence [[1..numStates1],[1..numStates2],[1..numStates3]]




-- Help function to create empty configuration from int-list. Only needed for initial configuration and debugging
toConf :: [Word8] -> C
toConf l = Conf (pack l) (L.map B2.pack ["", ""])

