import Data.List
import Data.Set
import Data.Char
data C = Conf [Int] [[Char]] | Null    deriving (Show, Eq, Ord)
data R = Rule [(Int, Int, Int)] [(Int, [Char], [Char])]
-- Att göra senare: Titta på "boundary retransmission"
-- Sliding window är beskrivet av tannenbaum

--set 5: 3.2 6: 7.6, 7: 16.4

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y
skriv a = putStrLn(show(a))
unionMap f = Data.Set.foldr (Data.Set.union . f) Data.Set.empty
combinations n = Data.List.filter (\s -> length s == n) . subsequences -- Not used

--Subwords of a set
getWordInterval a b l = take b (drop a l)

subwordK :: Int -> [Char] -> [[Char]]
subwordK 0 l = [""]
subwordK k l = nub [getWordInterval x k l | x <- [0..(length l -k)]]++subwordK (k-1) l
--subwordK k l = nub [take b (drop a l) | a <- [0..(length l)], b <- [0..k]]

cartProd' :: [Char] -> [[Char]] -> [[Char]]
cartProd' l1 l2 = [x++l1 | x <- l2]

cartProd :: [[Char]] -> [[Char]] -> [[[Char]]]
cartProd l1 l2 = sequence [cartProd' x symbols | x <- l1]

longerwords' :: C -> Set C
longerwords' (Conf sl chl) = fromList ([Conf sl x | x <- (cartProd chl symbols)])
longerwords :: Set C -> Set C
longerwords l = unionMap longerwords' l

apply :: (Set C) -> [R] -> (Set C)
apply setOfNodes listOfRules = unions [Data.Set.map (changeConf x) setOfNodes | x <- listOfRules]

gammaC :: (Set C) -> Int -> C -> C
gammaC l k c =
  let a = alpha k c in
    if'
    ((a Data.Set.\\ l) == empty)
    c
    Null

gammaD :: (Set C) -> Int -> (Set C)
gammaD l k = Data.Set.map (gammaC l k) (longerwords l)


alpha :: Int -> C -> (Set C)
alpha _ Null = empty
alpha k (Conf sl chl) =
    fromList [Conf sl x | x <- (sequence (Data.List.map (subwordK k) chl))]


alphaL :: (Set C) -> Int -> (Set C)
alphaL l k =(unions [alpha k  x | x <- (toList l)])



alg' :: (Set C) -> [R] -> Int -> (Set C)
alg' l r k = Data.Set.union (alphaL (apply ((gammaD l k)) r) k)  l


alg :: (Set C) -> [R] -> Int -> Int ->(Set C)
alg l r k 0 = l
alg l r k counter =
    let a = alg' l r k in
    if'
    ((size a) == size l)
    l
    (alg a r k (counter-1))



--runProgram :: [C] -> Int -> [C]
--runProgram l 0 = l
--runProgram l k = runProgram ((apply l transitions) `Data.Set.union` l) (k-1)

replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs


changeState :: (Int, Int, Int) -> [Int] -> [Int]
changeState (a, b, c) states =
  if (states!!(a-1) == b) then replaceNth (a-1) c states else []

changeChannel :: (Int, [Char], [Char]) -> [[Char]] -> [[Char]]
changeChannel (chanNum, "¡", symbol) channels =
  replaceNth (chanNum-1) (symbol++(channels!!(chanNum-1))) channels
changeChannel (chanNum, "!", symbol) channels =
    replaceNth (chanNum-1) ((channels!!(chanNum-1))++symbol) channels
changeChannel (chanNum, "?", symbol) channels =
  let ch = (channels!!(chanNum-1)) in
  if (ch /= "" && [head ch] == symbol)
  then replaceNth (chanNum-1) (tail ch) channels else []
                               
                               
changeConf :: R -> C -> C
changeConf _ Null = Null
changeConf (Rule _ _) (Conf _ []) = Null
changeConf (Rule _ _) (Conf [] _) = Null
changeConf (Rule [] []) c = c
changeConf (Rule [] (h:channelRules)) (Conf states channels) =
  changeConf (Rule [] channelRules) (Conf states (changeChannel h channels))
changeConf (Rule (h:stateRules) channelRules) (Conf states channels) =
  changeConf (Rule stateRules channelRules) (Conf (changeState h states) channels)




kor = (alg (fromList initial) transitions 5 50)
main = skriv (size kor)
--main = skriv (changeConf r2 (head initial))
--main = skriv (checkRule l2 (Conf [4,3] ["bbb", "aaa"]))
--main = skriv (confToTuples (checkRule s1 (checkRule s0 (head initial))))
--main = skriv (apply (gammaC (fromList initial) (longerwords (fromList initial)) 2) transitions)

initial = [Conf [1,1] ["",""]]


symbols :: [[Char]]
symbols = ["a","b",""]

r1 = Rule [(1,1,2)] []
r2 = Rule [(1,2,2)] [(1, "!", "a")]
r3 = Rule [(1,2,3)] [(2, "?", "a")]
r4 = Rule [(1,2,2)] [(2, "?", "b")]
r5 = Rule [(1,3,4)] []
r6 = Rule [(1,4,4)] [(1, "!", "b")]
r7 = Rule [(1,4,4)] [(2, "?", "a")]
r8 = Rule [(1,4,1)] [(2, "?", "b")]

r9 = Rule [(2,1,1)] [(1, "?", "b")]
r10 = Rule [(2,1,1)] [(2, "!", "b")]
r11 = Rule [(2,1,2)] [(1, "?", "a")] 
r12 = Rule [(2,2,3)] []
r13 = Rule [(2,3,3)] [(1, "?", "a")]
r14 = Rule [(2,3,3)] [(2, "!", "a")]
r15 = Rule [(2,3,4)] [(1, "?", "b")]
r16 = Rule [(2,4,1)] []


transitions = [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16]