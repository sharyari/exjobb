module StringManipulation where

import Data.HashSet as S
import Data.List as L
import Data.ByteString  as B
import DataTypes
import qualified Data.ByteString.Char8 as B2


-- This takes out an interval of a string/List
getWordInterval a b = (B.take b) . B.drop a

-- This is a help function for subwords
-- This can be used as a simple version of subwords
chooseK :: Int -> ByteString -> [ByteString]
chooseK k l =  let len = B2.length l in if (len >= k) then [getWordInterval a k l | a<-[0..(len-k)]] else [l]

-- This returns all subwords of size up to k of a word
subwords' :: Int -> ByteString -> [ByteString]
subwords' 0 l = []
subwords' k l =
    chooseK k l ++ subwords' (k-1) l

subwords k l = S.toList $ S.fromList $ subwords' k l

-- This returns all views of a configuration, with the states abstracted (as views does not affect them)
-- subwords is slower, but I finally found a situation where it created more configurations
--views :: Int -> [ByteString] -> [[ByteString]]      --TNode

views :: TNode -> Int -> [ByteString] -> [[ByteString]]
views node k sl = if S.member sl node then [] else sl:views' node (k-1) sl

views' node 0 sl = []
views' node k sl = L.concat $ L.map (views node k) (sequence $ [[sl!!0], chooseK k (sl!!1)])++L.map (views node k) (sequence $ [chooseK k (sl!!0), [sl!!1]])


simpleViews :: Int -> [ByteString] -> TNode
simpleViews k sl = S.fromList (sequence $ L.map (chooseK k) sl) -- subwords is slower


-- 17.6 s


