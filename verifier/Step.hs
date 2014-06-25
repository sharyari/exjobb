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


step :: (CTrie, [C]) -> (CTrie, [C])
step (trie, confs) = (trie, L.concat (L.map (applyRules rules) confs))







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


--s1 = ([(0,2,2)], (0,"!","a"))
--s2 = ([(0,2,2)], (1,"?","b"))
--s3 = ([(0,2,3)], (1,"?","a"))
--s5 = ([(0,4,4)], (0,"!","b"))
--s6 = ([(0,4,4)], (1,"?","a"))
--s7 = ([(0,4,1)], (1,"?","b"))
--r0 = ([(1,1,2)], (0,"?","a"))
--r1 = ([(1,1,1)], (0,"?","b"))
--r2 = ([(1,1,1)], (1,"!","b"))
--r4 = ([(1,3,4)], (0,"?","b"))
--r5 = ([(1,3,3)], (0,"?","a"))
--r6 = ([(1,3,3)], (1,"!","a"))
--sync0 = ([(2,1,2),(0,1,2)], (1,"_","b"))
--sync1 = ([(2,1,2),(0,3,4)], (1,"_","b"))
--sync2 = ([(2,2,3),(0,1,2)], (1,"_","b"))
--sync3 = ([(2,2,3),(0,3,4)], (1,"_","b"))
--sync4 = ([(2,2,1),(1,2,3)], (1,"_","b"))
--sync5 = ([(2,2,1),(1,4,1)], (1,"_","b"))
--sync6 = ([(2,1,3),(1,2,3)], (1,"_","b"))
--sync7 = ([(2,1,3),(1,4,1)], (1,"_","b"))

--transitions = [s1,s2,s3,s5,s6,s7,r0,r1,r2,r4,r5,r6,sync0,sync1,sync2,sync3,sync4,sync5,sync6,sync7]
s1 = ([(0,2,2)], (0,"!","a"))
s3 = ([(0,3,3)], (0,"!","ba"))
s4 = ([(0,2,4)], (1,"?","a"))
s5 = ([(0,3,5)], (1,"?","a"))
s8 = ([(0,5,5)], (0,"!","b"))
s9 = ([(0,5,7)], (1,"?","b"))
s10 = ([(0,6,8)], (1,"?","b"))
s13 = ([(0,8,8)], (0,"!","c"))
s14 = ([(0,9,9)], (0,"!","ac"))
s15 = ([(0,8,1)], (1,"?","c"))
s16 = ([(0,9,2)], (0,"?","c"))
s17 = ([(0,6,6)], (0,"!","cb"))
r0 = ([(1,1,1)], (1,"!","c"))
r1 = ([(1,1,2)], (0,"?","a"))
r3 = ([(1,3,3)], (1,"!","a"))
r4 = ([(1,3,4)], (0,"?","b"))
r6 = ([(1,5,5)], (1,"!","b"))
r7 = ([(1,5,6)], (0,"?","c"))
sync0 = ([(2,1,2),(0,1,2)], (1,"_","b"))
sync1 = ([(2,1,2),(0,2,3)], (1,"_","b"))
sync2 = ([(2,1,2),(0,4,5)], (1,"_","b"))
sync3 = ([(2,1,2),(0,5,6)], (1,"_","b"))
sync4 = ([(2,1,2),(0,7,8)], (1,"_","b"))
sync5 = ([(2,1,2),(0,8,9)], (1,"_","b"))
sync6 = ([(2,2,3),(0,1,2)], (1,"_","b"))
sync7 = ([(2,2,3),(0,2,3)], (1,"_","b"))
sync8 = ([(2,2,3),(0,4,5)], (1,"_","b"))
sync9 = ([(2,2,3),(0,5,6)], (1,"_","b"))
sync10 = ([(2,2,3),(0,7,8)], (1,"_","b"))
sync11 = ([(2,2,3),(0,8,9)], (1,"_","b"))
sync12 = ([(2,3,4),(0,1,2)], (1,"_","b"))
sync13 = ([(2,3,4),(0,2,3)], (1,"_","b"))
sync14 = ([(2,3,4),(0,4,5)], (1,"_","b"))
sync15 = ([(2,3,4),(0,5,6)], (1,"_","b"))
sync16 = ([(2,3,4),(0,7,8)], (1,"_","b"))
sync17 = ([(2,3,4),(0,8,9)], (1,"_","b"))
sync18 = ([(2,2,1),(1,2,3)], (1,"_","b"))
sync19 = ([(2,2,1),(1,4,5)], (1,"_","b"))
sync20 = ([(2,2,1),(1,6,1)], (1,"_","b"))
sync21 = ([(2,3,2),(1,2,3)], (1,"_","b"))
sync22 = ([(2,3,2),(1,4,5)], (1,"_","b"))
sync23 = ([(2,3,2),(1,6,1)], (1,"_","b"))
sync24 = ([(2,1,4),(1,2,3)], (1,"_","b"))
sync25 = ([(2,1,4),(1,4,5)], (1,"_","b"))
sync26 = ([(2,1,4),(1,6,1)], (1,"_","b"))

transitions = [s1,s3,s4,s5,s8,s9,s10,s13,s14,s15,s16,s17,r0,r1,r3,r4,r6,r7,sync0,sync1,sync2,sync3,sync4,sync5,sync6,sync7,sync8,sync9,sync10,sync11,sync12,sync13,sync14,sync15,sync16,sync17,sync18,sync19,sync20,sync21,sync22,sync23,sync24,sync25,sync26]


-- Help function to create empty configuration from int-list. Only needed for initial configuration and debugging
toConf :: [Word8] -> C
toConf l = Conf (pack l) ["", ""]


initial = toConf ([1,1,1])


rules = createRuleTree (L.concat (L.map translate transitions))
