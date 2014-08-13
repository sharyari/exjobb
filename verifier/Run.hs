module Run where
import DataTypes
import Data.Trie as T
import Prelude as P
import Data.Maybe (fromMaybe, fromJust, isJust)
import Data.List as L

import Data.ByteString
import Data.HashMap.Strict as S
import qualified Data.ByteString.Char8 as B2
import Debug.Trace

run :: ([C]) -> Trie [R] -> Int -> HashMap C C
run (a1) b c = run' (S.fromList (L.zip a1 (L.take 4 $ repeat (B2.empty,[]))),S.empty) b c 70

--check [] = ()
--check ((states, chans):xs) = if ((unpack states)!!2 == 4 ) then (states, chans) else check xs


run' :: (HashMap C C, HashMap C C) -> Trie [R] -> Int -> Int -> HashMap C C
run' (new,old) _ _ 0 =  old
run' (new, old) rules k i
  | new == S.empty = old
  | otherwise  = 
  let new' = S.difference (iteration rules (S.toList new) k) new in  
    run' (new', S.union old new') rules k (i-1)

iteration :: Trie [R] -> [(C,C)] -> Int -> HashMap C C
iteration r c k = S.fromList (L.concat (L.map (applyRules r k) c))

applyRules :: Trie [R] -> Int -> (C,C) -> [(C,C)]
applyRules trie k ((states, chan), parent) =
  let
    s = T.lookup states trie
  in
    if (isJust s) then
      L.concat [applyRule states chan parent x k | x <- (fromJust s)]
    else []

--applyRule :: C -> R -> C
--applyRule Null _ = Null
applyRule :: ByteString -> Eval -> C -> R -> Int -> [(C,C)]
applyRule states chan parent (Rule newState (i, "_", symbol)) k =
  [((newState, chan), (states, chan))]
applyRule states chan parent (Rule newState (i, "?", symbol)) k =
  if (P.length (chan!!i) > 0 && [P.last (chan!!i)] == symbol) then
  [((newState, (replaceNth i k (P.init (chan!!i))  chan)) ,(states, chan))] else []
applyRule states chan parent (Rule newState (i, "¡", symbol)) k =
  [((newState, (replaceNth i k (chan!!i++symbol) chan)), (states, chan))]
applyRule states chan parent (Rule newState (i, "!", symbol)) k =
  [((newState, (replaceNth i k (symbol++chan!!i) chan)), (states, chan))]


replaceNth n k newVal l =
  let diff = (P.length newVal - k) in
  if (diff > 0) then replaceNth' n (L.take k newVal) l
   else replaceNth' n newVal l

-- This is a function that replaces the nth value of a list
replaceNth' n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth' (n-1) newVal xs
