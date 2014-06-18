import Data.List
import Data.Set
import Data.Char
data C = Conf [Int] [[Char]] | Null    deriving (Show, Eq, Ord)
data R = Rule [(Int, Int)] [(Int, [Char])] [(Int, Int)] [(Int, [Char], [Char])]
-- Att göra senare: Titta på "boundary retransmission"
-- Sliding window är beskrivet av tannenbaum

--arr 5: 12.4, 6: 36.9, 7: 106
--set 5: 3.2 6: 7.6, 7: 16.4
--lapset 7:32

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y
skriv a = putStrLn(show(a))

--Subwords of a set
getWordInterval a b l = take (b-a) (drop a l)

subwordK 0 l = [""]
subwordK k l = nub [getWordInterval x (x+k) l | x <- [0..(length l -k)]]++subwordK (k-1) l


symbols :: [Char]
symbols = ['a','b']

cartProd l1 l2 = [Data.List.map (x:) l1 | x <- l2]
longerwords'' :: C -> Set C
--longerwords'' (Conf a b c d) = fromList [Conf a b c d, Conf a b ('a':c) ('a':d),Conf a b ('a':c) ('b':d), Conf a b ('b':c) ('a':d),Conf a b ('b':c) ('b':d)]
longerwords'' (Conf sl chl) = fromList [Conf sl x | x <- (cartProd chl symbols)]

unionMap f = Data.Set.foldr (Data.Set.union . f) Data.Set.empty

longerwords :: Set C -> Set C
longerwords l = unionMap longerwords'' l

apply :: (Set C) -> [C -> C] -> (Set C)
apply setOfNodes listOfRules = unions [Data.Set.map x setOfNodes | x <- listOfRules]


gammaC' :: (Set C) -> Int -> C -> C
gammaC' l k c =
  let a = alpha k c in
    if'
    ((a Data.Set.\\ l) == empty)
    c
    Null

gammaC :: (Set C) -> (Set C) -> Int -> (Set C)
gammaC l s k = Data.Set.map (gammaC' l k) s


gammaD :: (Set C) -> Int -> (Set C)
gammaD l k = gammaC l (longerwords l) k


alpha :: Int -> C -> (Set C)
alpha _ Null = empty
alpha k (Conf sl chl) =
    fromList [Conf sl x | x <- (sequence (Data.List.map (subwordK k) chl))]

alphaL :: (Set C) -> Int -> (Set C)
alphaL l k =(unions [alpha k  x | x <- (toList l)])



alg' :: (Set C) -> [C->C] -> Int -> (Set C)
alg' l r k = (alphaL (apply ((gammaD l k)) r)) k  `Data.Set.union` l


alg :: (Set C) -> [C->C] -> Int -> Int ->(Set C)
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

confToTuples'' :: Int -> [[Char]] -> [(Int, [Char])]
confToTuples'' k [] = []
confToTuples'' k ("":tl) = (k, ""):confToTuples'' (k+1) tl
confToTuples'' k (h:tl) = (k, [head h]):confToTuples'' (k+1) tl

confToTuples' :: Int -> [Int] -> [(Int, Int)]
confToTuples' k [] = []
confToTuples' k (h:tl) = (k, h):confToTuples' (k+1) tl

confToTuples :: C -> ([(Int, Int)], [(Int, [Char])])
confToTuples (Conf il chl) = ((confToTuples' 1 il), (confToTuples'' 1 chl))


checkRule :: C -> R -> C
checkRule c (Rule l1 l2 l3 l4) =
  let ts = confToTuples c in 
  if (l1 Data.List.\\ (fst ts) == [] && l2 Data.List.\\ (snd ts) == []) then changeConf ts l3 l4 else Null

changeState _ [] = []
changeState [] _ = []
changeState ((a,b):il) ((c,d):nil) = if (a==c) then d : (changeState il nil) else b:changeState il ((c,d):nil)


changeChannel _ [] = []
changeChannel [] _ = []
changeChannel ((a,b):il) ((c,"?", d):nil) = if (a==c) then (tail b) : (changeChannel il nil) else b:changeChannel il ((c,"?",d):nil)
changeChannel ((a,b):il) ((c,"!", d):nil) = if (a==c) then (d++b) : (changeChannel il nil) else b:changeChannel il ((c,"!",d):nil)

changeConf :: ([(Int, Int)], [(Int, [Char])]) -> [(Int, Int)] -> [(Int, [Char], [Char])] -> C
changeConf (il, chl) nil nchl = Conf (changeState il nil) (changeChannel chl nchl)
                                                  
kor = alg (fromList initial) transitions 7 50
--main = skriv (size kor) 

main = skriv(checkRule (head initial) r1)

r1 :: R
r1 = Rule [(1,1)] [(1, "a")] [(1,2)] [(1, "!", "a")]

transitions = []
initial = [Conf [1] ["a",""]]