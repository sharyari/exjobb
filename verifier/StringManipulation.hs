module StringManipulation where

import Data.Set as S
import Data.List as L


symbols = ["a","b",""]

-- This takes out an interval of a string/List
getWordInterval a b l = take b (drop a l)

-- This is a help function for subwords
-- This can be used as a simple version of subwords
chooseK :: Int -> [b] -> [[b]]
chooseK k l       =  if (length l >= k) then [getWordInterval a (a+k) l | a<-[0..(length l-k)]] else [l]

-- This returns all subwords of size up to k of a word
subwords :: Int -> String -> [String]
subwords 0 l = [""]
subwords k l = chooseK k l ++ subwords (k-1) l

-- This returns all views of a configuration, with the states abstracted (as views does not affect them)
views :: Int -> [String] -> [[String]]
views k sl = sequence (L.map (subwords k) sl)

-- This returns all "simple" views of a configuration
simpleViews :: Int -> [String] -> Set [String]
simpleViews k sl = S.fromList (sequence (L.map (chooseK k) sl))

--Given a channel evaluation, this returns all channel evaluation with length at most 1 more
longer :: [String] -> Set [String]
longer sl = S.fromList (sequence [[x++y | y<-symbols] | x <-sl])






