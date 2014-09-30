module UnOrdered where

import Data.ByteString as B
import Data.Set as S
import Data.Maybe (fromMaybe, fromJust, isJust)
import Data.HashMap.Strict as M

import DataTypes
import HashMapModule
import ProblemFormulation
import Data.Word
import Debug.Trace

-- This function create a large list of repeated values
takeRepeat a = Prelude.take 1000000 $ repeat a

-- This is a function that replaces the nth value of a list
replaceNth n newVal (x:xs)
 | n == 0 = newVal:xs
 | otherwise = x:replaceNth (n-1) newVal xs

-- This function creates an integer from an integer list
fromDigits :: CWord -> Word8
fromDigits = Prelude.foldl addDigit 0
   where addDigit num d = 10*num + d


-- This functions checks if a configuration is bad
isBadConfiguration bad (state,eval) = or [index state x == y | (x,y) <- bad]

-- This function creates a minimal trace
traceBad set initial ((state, chan)) =
  if (state == B.empty || (state,chan) == initial) then
    ((show $ B.unpack state) ++ show (Prelude.map (fromDigits) chan))
  else
    traceShow ((show $ B.unpack state) ++ show (Prelude.map (fromDigits) chan))
    traceBad set initial (fromMaybe (B.empty,[]) $ M.lookup (state,chan) set)

------------------------------------------------------
--------------- SECTION INITIALIZATION ---------------
------------------------------------------------------

-- This function creates a trie of rules, indexed by state
createRuleTree :: [(CWord, CWord, (Int, String, CWord))] -> RuleMap
createRuleTree [] = M.empty
createRuleTree ((w1,w2,tuple):xs) = tAddRule (createRuleTree xs) ((B.pack w1,B.pack w2, tuple))

-- This function inspects a rule in its original format, and creates a list of
-- rules in the format used by the ruletrie
translate :: ([(Int,Int,Int)], (Int, String, CWord)) -> [(CWord, CWord, (Int, String, CWord))]
translate (ilist,tuple) =
  let
    res = Prelude.filter (checkPred ilist) (combs numStates)
  in
   [(Prelude.map toW8 x, Prelude.map toW8 (perform ilist x), tuple) | x <- res]

-- This function translates an integer to a word8
toW8 :: Int -> Word8
toW8 i = fromInteger (toInteger i)

-- Can't remember. Something about translating I guess
perform :: [(Int, Int, Int)] -> [Int] -> [Int]
perform [] il = il
perform ((a,b,c):tl) il = perform tl (replaceNth a c il)

-- Check if the global state confirms to the state-predicate of a rule
checkPred :: [(Int, Int, Int)] -> [Int] -> Bool
checkPred [] _ = True
checkPred ((a,b,c):l) il = if ((il!!a) /= b ) then False else checkPred l il

-- create the cartesian product of possible global states
combs :: [Int] -> [[Int]]
combs ints = sequence $ [[1..x] | x<- ints]

-- Short-hand notation. Creates a rule tree
rules = createRuleTree (Prelude.concat (Prelude.map translate transitions))
-- Short-hand notation. Creates a configuration tree containing only the initial configuration
myTrie = mapAdd M.empty (toConf initial)
-- Short-hand notation. Creates a configuration as a configuration-parent pair
myConf = ([toConf initial], [])

-- Help function to create empty configuration from int-list. Only needed for initial configuration and debugging
toConf :: CWord -> C
toConf l = (B.pack l, Prelude.take (Prelude.length symbols) $ repeat [])
