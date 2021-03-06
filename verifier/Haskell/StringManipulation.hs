module StringManipulation where

import Data.HashSet as S
import qualified Data.ByteString.Char8 as B2

import DataTypes
import Data.List as L

-- This is a help function for subwords
-- This can be used as a simple version of subwords
chooseK :: Int -> CWord -> [CWord]
chooseK k l =
    if (length l > k)
    then
      L.map (\x -> (take k . drop x) l)  [0..(length l-k)]
    else
      []

-- This returns all views of a configuration, with the states abstracted (as views does not affect them)
-- views and views' call eachother; views checks if an evaluation is already in the node, if it is, ignore it,
-- otherwise, add it to the list of new eval, and use views' to create its subwords of size k-1

views :: MapNode -> Int -> Eval -> [Eval]
views node k sl =
  if S.member sl node then
    []
  else 
    sl:views' node (k-1) sl

-- Views found an evaluation that hasn't been seen before. Create all evaluation smaller of size take
-- of that evaluation, and run views on them.
views' :: MapNode -> Int -> Eval -> [Eval]
views' node 0 sl = []
views' node k sl =
    concatMap (views node k) $ concatMap sequence [test x k sl | x <- [0..(L.length sl-1)]]
--(sequence [[sl!!0], chooseK k (sl!!1)] ++ sequence [chooseK k (sl!!0), [sl!!1]])

test _ k [] = []
test 0 k (s:sl) = (chooseK k s):test (-1) k sl
test n k (s:sl) =  [s]:test (n-1) k sl
