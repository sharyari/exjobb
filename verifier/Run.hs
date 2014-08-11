module Run where
import DataTypes
import Data.Trie as T
import Prelude as P
import Data.Maybe (fromMaybe, fromJust, isJust)
import Data.List as L
import Data.Set as S
import Data.ByteString
import qualified Data.ByteString.Char8 as B2
import Debug.Trace

run :: ([C],[C]) -> Trie [R] -> Int -> Set C
run (a1,a2) b c = snd (run' (S.fromList a1,S.fromList a2) b c 70)

--check [] = ()
--check ((states, chans):xs) = if ((unpack states)!!2 == 4 ) then (states, chans) else check xs

run' :: (Set C, Set C) -> Trie [R] -> Int -> Int -> (Set C, Set C)
run' (new,old) _ _ 0 = (new, old)
run' (new, old) rules k i
  | new == S.empty = (S.empty, old)
  | otherwise  = run' (iteration rules (S.toList (new S.\\ old)) k, S.union new old) rules k (i-1)

iteration :: Trie [R] -> [C] -> Int -> Set C
iteration r c k = S.fromList (L.concat (L.map (applyRules r k) c))

applyRules :: Trie [R] -> Int -> C -> [C]
applyRules trie k (states, chan) = let s = T.lookup states trie in
  if (isJust s) then
    L.concat [applyRule states chan x k | x <- (fromJust s)]
  else []

--applyRule :: C -> R -> C
--applyRule Null _ = Null
applyRule :: ByteString -> Eval -> R -> Int -> [C]
applyRule states chan (Rule newState (i, "_", symbol)) k =
  [(newState, chan)]
applyRule states chan (Rule newState (i, "?", symbol)) k =
  if (P.length (chan!!i) > 0 && [P.last (chan!!i)] == symbol) then
  [(newState, replaceNth i k (P.init (chan!!i))  chan)] else []
applyRule states chan (Rule newState (i, "¡", symbol)) k =
  [(newState, (replaceNth i k (chan!!i++symbol) chan))]
applyRule states chan (Rule newState (i, "!", symbol)) k =
  [(newState, (replaceNth i k (symbol++chan!!i) chan))]

replaceNth n k newVal l =
  let diff = (P.length newVal - k) in
  if (diff > 0) then replaceNth' n (L.take k newVal) l
   else replaceNth' n newVal l

-- This is a function that replaces the nth value of a list
replaceNth' n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth' (n-1) newVal xs
