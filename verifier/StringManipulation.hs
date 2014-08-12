module StringManipulation where

import Data.HashSet as S
import Data.List as L
import DataTypes
import qualified Data.ByteString.Char8 as B2
import Debug.Trace

-- This takes out an interval of a string/List
getWordInterval a b = (L.take b) . L.drop a

-- This is a help function for subwords
-- This can be used as a simple version of subwords
chooseK :: Int -> CWord -> [CWord]
chooseK k l =  let len = L.length l in if (len >= k) then [getWordInterval a k l | a<-[0..(len-k)]] else [l]

-- This returns all subwords of size up to k of a word
subwords' :: Int -> CWord -> [CWord]
subwords' 0 l = []
subwords' k l =
    chooseK k l ++ subwords' (k-1) l

-- This returns all views of a configuration, with the states abstracted (as views does not affect them)
-- views and views' call eachother; views checks if an evaluation is already in the node, if it is, ignore it,
-- otherwise, add it to the list of new eval, and use views' to create its subwords of size k-1
views :: TNode -> Int -> Eval -> [Eval]
views node k sl = if S.member sl node then [] else sl:views' node (k-1) sl

views' :: TNode -> Int -> Eval -> [Eval]
views' node 0 sl = []
views' node k sl =
    L.concatMap (views node k) $  (sequence $ [[sl!!0], chooseK k (sl!!1)])++(sequence $ [chooseK k (sl!!0), [sl!!1]])

-- This is a function that replaces the nth value of a list
replaceNth n newVal (x:xs)
 | n == 0 = newVal:xs
 | otherwise = x:replaceNth (n-1) newVal xs
