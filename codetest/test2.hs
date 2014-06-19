import Data.List
import Data.Set
import Data.Char
data C = Conf [Int] [[Char]] | Null    deriving (Show, Eq, Ord)
data R = Rule [(Int, Int)] [(Int, [Char])] [(Int, Int)] [(Int, [Char], [Char])]
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
apply setOfNodes listOfRules = unions [Data.Set.map (checkRule x) setOfNodes | x <- listOfRules]

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

--For recreating configurations
confToTuples'' :: Int -> [[Char]] -> [(Int, [Char])]
confToTuples'' _ [] = []
confToTuples'' k ("":tl) = (k, ""):confToTuples'' (k+1) tl
confToTuples'' k (h:tl) = (k, h):confToTuples'' (k+1) tl


--For checking correctness
confToTuples''' :: Int -> [[Char]] -> [(Int, [Char])]
confToTuples''' _ [] = []
confToTuples''' k ("":tl) = (k, ""):confToTuples'' (k+1) tl
confToTuples''' k (h:tl) = (k, [last h]):confToTuples'' (k+1) tl

--For ints
confToTuples' :: Int -> [Int] -> [(Int, Int)]
confToTuples' _ [] = []
confToTuples' k (h:tl) = (k, h):confToTuples' (k+1) tl

confToTuples :: C -> ([(Int, Int)], [(Int, [Char])])
confToTuples (Conf il chl) = ((confToTuples' 1 il), (confToTuples'' 1 chl))
confToTuples Null = ([],[])

confToTuples2 :: C -> ([(Int, Int)], [(Int, [Char])])
confToTuples2 (Conf il chl) = ((confToTuples' 1 il), (confToTuples''' 1 chl))
confToTuples2 Null = ([],[])



checkRule :: R -> C -> C
checkRule (Rule l1 l2 l3 l4) c=
  let ts = confToTuples2 c in
  if (l1 Data.List.\\ (fst ts) == [])  && (l2 Data.List.\\ (snd ts)) == [] then changeConf (confToTuples c) l3 l4 else Null

changeState [] _ = []
changeState ((a,b):il) [] = b:changeState il []
changeState ((a,b):il) ((c,d):nil) = if (a==c) then d : (changeState il nil) else b:changeState il ((c,d):nil)

changeChannel' :: [(Int,[Char])] -> [(Int, [Char], [Char])] -> [[Char]]
changeChannel' [] _ = []
changeChannel' ((a,b):il) [] = b : changeChannel' il []
changeChannel' ((a,b):il) ((c,"?", d):nil) = if (a==c) then (tail b) : (changeChannel' il nil) else b:changeChannel' il ((c,"?",d):nil)
changeChannel' ((a,b):il) ((c,"!", d):nil) = if (a==c) then (d++b) : (changeChannel' il nil) else b:changeChannel' il ((c,"!",d):nil)
changeChannel' ((a,b):il) ((c,"¡", d):nil) = if (a==c) then (b++d) : (changeChannel' il nil) else b:changeChannel' il ((c,"!",d):nil)

changeChannel [] _ = [""]
changeChannel l1 l2 = changeChannel' l1 l2

changeConf :: ([(Int, Int)], [(Int, [Char])]) -> [(Int, Int)] -> [(Int, [Char], [Char])] -> C
changeConf (il, chl) nil nchl = Conf (changeState il nil) (changeChannel chl nchl)

kor = (alg (fromList initial) transitions 5 50 )
main = skriv (size kor)
--main = skriv (subwordK 2 "hejsan")
--main = skriv (checkRule l2 (Conf [4,3] ["bbb", "aaa"]))
--main = skriv (confToTuples (checkRule s1 (checkRule s0 (head initial))))
--main = skriv (apply (gammaC (fromList initial) (longerwords (fromList initial)) 2) transitions)

s0 = Rule [(1,1)] [] [(1,2)]  []
s1 = Rule [(1,2)] [] []  [(1,"!","a")]
s2 = Rule [(1,2)] [(2,"b")] []  [(2,"?","b")]
s3 = Rule [(1,2)] [(2,"a")] [(1,3)]  [(2,"?","a")]
s4 = Rule [(1,3)] [] [(1,4)]  []
s5 = Rule [(1,4)] [] []  [(1,"!","b")]
s6 = Rule [(1,4)] [(2,"a")] []  [(2,"?","a")]
s7 = Rule [(1,4)] [(2,"b")] [(1,1)]  [(2,"?","b")]
r0 = Rule [(2,1)] [(1,"a")] [(2,2)]  [(1,"?","a")]
r1 = Rule [(2,1)] [(1,"b")] []  [(1,"?","b")]
r2 = Rule [(2,1)] [] []  [(2,"!","b")]
r3 = Rule [(2,2)] [] [(2,3)]  []
r4 = Rule [(2,3)] [(1,"b")] [(2,4)]  [(1,"?","b")]
r5 = Rule [(2,3)] [(1,"a")] []  [(1,"?","a")]
r6 = Rule [(2,3)] [] []  [(2,"!","a")]
r7 = Rule [(2,4)] [] [(2,1)]  []

l0 = Rule [] [(1,"a")] [] [(1, "?", "s")]
l1 = Rule [] [(2, "a")] [] [(2, "?", "s")]
l2 = Rule [(1,4)] [(1,"b")] [] [(1, "?", "b")]
l3 = Rule [] [(2, "b")] [] [(2, "?", "s")]


initial = [Conf [1,1] ["",""]]
transitions = [s0,s1,s2,s3,s4,s5,s6,s7,r0,r1,r2,r3,r4,r5,r6,r7,l0,l1,l2,l3]

symbols :: [[Char]]
symbols = ["a","b",""]
