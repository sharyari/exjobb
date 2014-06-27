module StringManipulation where

import Data.HashSet as S
import Data.List as L
import Data.ByteString as B
import DataTypes
import qualified Data.ByteString.Char8 as B2

symbols = L.map B2.pack ["a","b", "c"]

--symbols= L.map B2.pack [""]

-- This takes out an interval of a string/List
getWordInterval a b l = B.take b (B.drop a l)

-- This is a help function for subwords
-- This can be used as a simple version of subwords
chooseK :: Int -> ByteString -> [ByteString]
chooseK k l =  if (B.length l >= k) then [getWordInterval a k l | a<-[0..(B.length l-k)]] else [l]

-- This returns all subwords of size up to k of a word
subwords :: Int -> ByteString -> [ByteString]
subwords 0 l = []
subwords k l = chooseK k l ++ subwords (k-1) l


-- This returns all views of a configuration, with the states abstracted (as views does not affect them)
-- subwords is slower, but I finally found a situation where it created more configurations
views :: Int -> [ByteString] -> TNode
views k sl = S.fromList ([x | x <- (sequence (L.map (subwords k) sl))])

simpleViews :: Int -> [ByteString] -> TNode
simpleViews k sl = S.fromList ([x | x <- (sequence (L.map (chooseK k) sl))]) -- subwords is slower

--Given a channel evaluation, this returns all channel evaluation with length at most 1 more
longer :: [ByteString] -> [[ByteString]]
longer sl = sequence [[B.concat [x,y] | y<-symbols] | x <-sl]





