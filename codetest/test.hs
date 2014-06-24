import Data.List
import Data.Set
import Data.Char
data C = Conf [Int] [[Char]] C| Null
data R = Rule [(Int, Int, Int)] [(Int, [Char], [Char])]
-- Att göra senare: Titta på "boundary retransmission"
-- Sliding window är beskrivet av tannenbaum

--set 5: 3.2 6: 7.6, 7: 16.4

instance Eq C where
  (Conf a b c) == (Conf d e f) = ((a==d) && (b==e))
  (Null) == (Null) = True
  _ == _ = False

instance Ord C where
  (Conf s1 s2 _) `compare` (Conf s3 s4 _) = ((s1,s2) `compare` (s3,s4))
  (Null) `compare` b = Prelude.LT
  a `compare` (Null) = Prelude.GT


instance Show C where
  show (Conf ints chars parent) =
    show ints ++ show chars ++ "\t\t" ++ show parent ++ "\n"
  show (Null) = ""

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y
qunionMap f = Data.Set.foldr (Data.Set.union . f) Data.Set.empty
combinations n = Data.List.filter (\s -> length s == n) . subsequences -- Not used

--Subwords of a set
getWordInterval a b l = take b (drop a l)

-- Using Choose is slower, but should prove faster if used correctly
chooseK :: Int -> [b] -> [[b]]
0      `chooseK` _       = [[]]
_     `chooseK` []       =  []
k `chooseK` (x:xs)       =  (x:) `fmap` ((k-1) `chooseK` xs) ++ k `chooseK` xs

choose :: Int -> [Char] -> [[Char]]
choose 0 l = [""]
choose k l = chooseK k l ++ choose (k-1) l

subwordK :: Int -> [Char] -> [[Char]]
subwordK 0 l = [""]
subwordK k l = nub [getWordInterval x k l | x <- [0..(length l -k)]]++subwordK (k-1) l
--subwordK k l = nub [take b (drop a l) | a <- [0..(length l)], b <- [0..k]]

cartProd' :: [Char] -> [[Char]] -> [[Char]]
cartProd' l1 l2 = [x++l1 | x <- l2]

cartProd :: [[Char]] -> [[Char]] -> [[[Char]]]
cartProd l1 l2 = sequence [cartProd' x symbols | x <- l1]

longerwords' :: C -> Set C
longerwords' (Conf sl chl parent) = fromList ([Conf sl x (Conf sl chl Null) | x <- (cartProd chl symbols)])
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
alpha k (Conf sl chl parent) =
    fromList [Conf sl x parent | x <- (sequence (Data.List.map (subwordK k) chl))]


alphaL :: (Set C) -> Int -> (Set C)
alphaL l k =(unions [alpha k  x | x <- (toList l)])



alg' :: (Set C) -> [R] -> Int -> (Set C)
alg' l r k = Data.Set.union l (alphaL (apply ((gammaD l k)) r) k)
--alg' l r k = alphaL (apply ((gammaD l k)) r) k


alg :: (Set C) -> [R] -> Int -> Int ->(Set C)
alg l r k 0 = fromList []
alg l r k counter =
    let a = alg' l r k in
    if'
    ((Data.Set.size a) == Data.Set.size l)
    l
    (alg a r k (counter-1))


getParent :: C -> C
getParent Null = Null
getParent (Conf a b c ) = c

findConf :: C -> [C] -> C
findConf c [] = Null
findConf c (h:tl) = if (c==h) then h else findConf c tl

trace :: C -> [C] -> [C]
trace Null list = []
trace c list = c:trace (getParent (findConf c list)) list

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


changeConf' :: R -> C -> C -> C
changeConf' _ Null _ = Null
changeConf' (Rule _ _) (Conf _ [] _) _ = Null
changeConf' (Rule _ _) (Conf [] _ _) _ = Null
changeConf' (Rule [] []) (Conf a b _) parent = (Conf a b parent)
changeConf' (Rule [] (h:channelRules)) (Conf states channels p) parent =
  changeConf' (Rule [] channelRules) (Conf states (changeChannel h channels) Null) parent
changeConf' (Rule (h:stateRules) channelRules) (Conf states channels p) parent =
  changeConf' (Rule stateRules channelRules) (Conf (changeState h states) channels Null) parent

changeConf :: R -> C -> C
changeConf r (Conf a b parent) = changeConf' r (Conf a b Null) parent
changeConf r Null = Null

checkForBad [] (Conf _ _ _)  = True
checkForBad _ (Conf [] _ _) = True
checkForBad ((a,b):bsl) (Conf il chl parent) = if (il!!(a-1)==b) then (checkForBad bsl (Conf il chl parent)) else False


kor2 = (alg (fromList initial) transitions 5 50)
--kor = Data.Set.filter (checkForBad bad) (alg (fromList initial) transitions 5 50)
--main = skriv (choose 2 "hej")
main = skriv (size kor2)
--main = skriv (trace (Conf [4,4,2] ["bb","aa"] Null) (toList kor2))
--main = skriv (changeConf r2 (head initial))
--main = skriv (checkRule l2 (Conf [4,3] ["bbb", "aaa"]))
--main = skriv (confToTuples (checkRule s1 (checkRule s0 (head initial))))
--main = skriv (Data.Set.map (gammaC (fromList initial) 2) (longerwords (fromList initial)))
--main = skriv (alphaL (apply (fromList initial) transitions) 2)
--main = skriv (gammaC (fromList initial) 2 (head initial))
--main = skriv ((alpha 2 (head initial)) `Data.Set.isSubsetOf` (fromList initial))
symbols :: [[Char]]
symbols = ["a","b",""]

bad = [(3,4)]


s1 = Rule [(1,2,2)] [(1,"!","a")]
s2 = Rule [(1,2,2)] [(2,"?","b")]
s3 = Rule [(1,2,3)] [(2,"?","a")]
s5 = Rule [(1,4,4)] [(1,"!","b")]
s6 = Rule [(1,4,4)] [(2,"?","a")]
s7 = Rule [(1,4,1)] [(2,"?","b")]
r0 = Rule [(2,1,2)] [(1,"?","a")]
r1 = Rule [(2,1,1)] [(1,"?","b")]
r2 = Rule [(2,1,1)] [(2,"!","b")]
r4 = Rule [(2,3,4)] [(1,"?","b")]
r5 = Rule [(2,3,3)] [(1,"?","a")]
r6 = Rule [(2,3,3)] [(2,"!","a")]
sync0 = Rule [(3,1,2),(1,1,2)] []
sync1 = Rule [(3,1,2),(1,3,4)] []
sync2 = Rule [(3,2,3),(1,1,2)] []
sync3 = Rule [(3,2,3),(1,3,4)] []
sync4 = Rule [(3,2,1),(2,2,3)] []
sync5 = Rule [(3,2,1),(2,4,1)] []
sync6 = Rule [(3,1,3),(2,2,3)] []
sync7 = Rule [(3,1,3),(2,4,1)] []

initial = [Conf [1,1,1] ["",""] Null]
transitions = [s1,s2,s3,s5,s6,s7,r0,r1,r2,r4,r5,r6,sync0,sync1,sync2,sync3,sync4,sync5,sync6,sync7]
