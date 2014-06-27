module Run where
--import Step
import DataTypes
import Data.Trie as T
import Prelude as P
import Data.Maybe (fromMaybe, fromJust, isJust)
import Data.List as L
import Data.Set as S
import Data.ByteString


run :: ([C],[C]) -> Trie [R] -> Int -> Set C
run (a1,a2) b c = (snd (run' (S.fromList a1,S.fromList a2) b c 70))

check [] = Null
check ((Conf states chans):xs) = if ((unpack states)!!2 == 4 ) then (Conf states chans) else check xs

run' :: (Set C, Set C) -> Trie [R] -> Int -> Int -> (Set C, Set C)
run' (new,old) _ _ 0 = (new, old)
run' (new, old) rules k i
  | new == S.empty = (S.empty, old)
  | otherwise     = run' (iteration rules (S.toList (new S.\\ old)) k, S.union new old) rules k (i-1)

iteration :: Trie [R] -> [C] -> Int -> Set C
iteration r c k = S.fromList (L.concat (L.map (applyRules r k) c))

applyRules :: Trie [R] -> Int -> C -> [C]
applyRules _ _ Null = [Null]
applyRules trie k (Conf states chan) = let s = T.lookup states trie in
  if (isJust s) then
    [applyRule (Conf states chan) x k | x <- (fromJust s)]
  else []

applyRule :: C -> R -> Int -> C
applyRule Null _ _ = Null
applyRule (Conf states chan) (Rule newState (i, "_", symbol)) _ =
  Conf newState chan
applyRule (Conf states chan) (Rule newState (i, "?", symbol)) k =
  if (L.length (chan!!i) > 0 && [P.head (chan!!i)] == symbol) then
  Conf newState (replaceNth i k (P.tail (chan!!i)) chan) else Null
applyRule (Conf states chan) (Rule newState (i, "!", symbol)) k =
  Conf newState (replaceNth i k (chan!!i++symbol) chan)
applyRule (Conf states chan) (Rule newState (i, "¡", symbol)) k =
  Conf newState (replaceNth i k (symbol++chan!!i) chan)

replaceNth n k newVal l =
  let diff = (P.length newVal - k) in
  if (diff > 0) then replaceNth' n (L.drop diff newVal) l
   else replaceNth' n newVal l

-- This is a function that replaces the nth value of a list
replaceNth' n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth' (n-1) newVal xs