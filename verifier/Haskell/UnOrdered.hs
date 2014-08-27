module UnOrdered where
import Data.Trie as T
import Data.ByteString as B
import Data.Set as S
import Data.Maybe (fromMaybe, fromJust, isJust)
import Data.HashMap.Strict as M
import DataTypes
import TrieModule
import ProblemFormulation
import StringManipulation
import Data.Word
import Debug.Trace
takeRepeat a = Prelude.take 1000000 $ repeat a

-- This is a function that replaces the nth value of a list
replaceNth n newVal (x:xs)
 | n == 0 = newVal:xs
 | otherwise = x:replaceNth (n-1) newVal xs

-- This function creates an integer from an integer list
fromDigits :: CWord -> Word8
fromDigits = Prelude.foldl addDigit 0
   where addDigit num d = 10*num + d


isBadState bad ((state, chan), parent) = or [index state x == y | (x,y) <- bad]
isBadConfiguration bad (state,eval) = or [index state x == y | (x,y) <- bad]

traceBad set initial ((state, chan)) =
  if (state == B.empty || (state,chan) == initial) then show () else
  traceShow ((show $ B.unpack state) ++ show (Prelude.map (fromDigits) chan))
  traceBad set initial (fromMaybe (B.empty,[]) $ M.lookup (state,chan) set)

------------------------------------------------------
--------------- SECTION INITIALIZATION ---------------
------------------------------------------------------
createRuleTree :: [(CWord, CWord, (Int, String, CWord))] -> RuleMap
createRuleTree [] = M.empty
createRuleTree ((w1,w2,tuple):xs) = tAddRule (createRuleTree xs) ((B.pack w1,B.pack w2, tuple))

translate :: ([(Int,Int,Int)], (Int, String, CWord)) -> [(CWord, CWord, (Int, String, CWord))]
translate (ilist,tuple) = let res = Prelude.filter (checkPred ilist) (combs numStates) in [(toW8 x, toW8 (perform ilist x), tuple) | x <- res]

toW8 :: [Int] -> CWord
toW8 [] = []
toW8 (i:l) = fromInteger (toInteger i) : toW8 l

perform :: [(Int, Int, Int)] -> [Int] -> [Int]
perform [] il = il
perform ((a,b,c):tl) il = perform tl (replaceNth a c il)

checkPred :: [(Int, Int, Int)] -> [Int] -> Bool
checkPred [] _ = True
checkPred ((a,b,c):l) il = if ((il!!a) /= b ) then False else checkPred l il

combs' :: Int -> [Int]
combs' x = [1..x]

combs :: [Int] -> [[Int]]
combs ints = sequence $ Prelude.map combs' ints

rules = createRuleTree (Prelude.concat (Prelude.map translate transitions))
myTrie = mapAdd M.empty (toConf initial)
myConf = ([toConf initial], [])


-- Help function to create empty configuration from int-list. Only needed for initial configuration and debugging
toConf :: CWord -> C
toConf l = (B.pack l, [[], []])
