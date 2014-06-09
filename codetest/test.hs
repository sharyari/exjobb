import Control.Parallel
import Data.List
import Data.Set
import Data.Char
data C = Conf Int Int [Char] [Char] | Null    deriving (Show, Eq, Ord)
data R = Rule (C->C)

--main = putStrLn(show (gamma 4 ["", "a", "b", "ab", "ba"] ["a","ba"]))
--main = putStrLn(show(alpha (Conf 1 2 "b" "cc"))
--main = putStrLn(show (nub (gammaC (alpha (Conf 1 2 "ab" "bc") 2) [Conf 1 2 "b" "cc"] 1 )))

--main = skriv (nub(apply [(Conf 1 2 "ab" "cd"),(Conf 1 2 "b" "cd"),(Conf 2 2 "ab" "cd")] [rule1,rule2]))

skriv a = putStrLn(show(a))



--Subwords of a set
getWordInterval a b l = take (b-a) (drop a l)
subword' a 0 len k l = if a < len then subword' (a+1) len len k l else [""]
subword' a b len k l = if (b-a)<=k then (getWordInterval a b l) : subword' a (b-1) len k l else subword' a (b-1) len k l
subword l k = subword' 0 (length l) (length l) k l

subwordOnlyK l k = [getWordInterval x (x+k) l | x <- [1..(length l -1)]]

--Words up to size k
cartProd xs ys = [x++y | x <- xs, y <- ys]
wordsk :: Int -> [[Char]] -> [[Char]]
wordsk 1 alphabet = alphabet
wordsk k alphabet = cartProd alphabet (wordsk (k-1) alphabet)

confsK a b k alphabet = [Conf a b x y | x <- (wordsk k alphabet), y <- (wordsk k alphabet)]

cartTuple :: [[Char]] -> [[Char]] -> [([Char],[Char])]
cartTuple xs ys = [(x,y) | x <- xs, y <- ys]

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

--Gamma
--gamma' ::  [[Char]] -> [[Char]] -> [[Char]]
--gamma' l [] = []
--gamma' l (word:list) = 
--    if'
--    (fromList (subword word 0) `isSubsetOf` (fromList l))
--    (word:gamma' l list)
--    (gamma' l list)

--gamma :: Int -> [[Char]] -> [[Char]] -> [[[Char]]]
--gamma 0 l alphabet = []
--gamma k l alphabet = gamma' l (wordsk k alphabet):gamma (k-1) l alphabet

gammaC l [] k = []
gammaC l (c:onf) k = 
    if' 
    ((fromList (alpha c k)) `isSubsetOf` (fromList l))
    (c:gammaC l onf k)
    (gammaC l onf k)

gammaD l k = gammaC l (nub (gammaC l (longerwords l) k)) k

longerwords'' Null = []
longerwords'' (Conf a b c d) = [(Conf a b ('a':c) d),(Conf a b ('b':c) d),(Conf a b ('a':c) ('a':d)),(Conf a b ('a':c) ('b':d)), (Conf a b ('b':c) ('a':d)),(Conf a b ('b':c) ('b':d)), (Conf a b c ('a':d)),(Conf a b c ('b':d))]


longerwords' [] = []
longerwords' (h:t) = (longerwords'' h)++longerwords' t

longerwords l = nub (longerwords' l)

alpha :: C -> Int -> [C]
alpha Null _ = []
alpha (Conf s r c1 c2) k =
    [Conf s r x y | x <- subwordOnlyK c1 k, y <- subwordOnlyK c2 k]

alphaL :: [C] -> Int -> [C]
alphaL [] _ = []
alphaL (h:t) k = (alpha h k)++alphaL t k



apply' :: [C] -> (C -> C) -> [C]
apply' [] _ = []
apply' (h:t) r = (r h):apply' t r 

apply :: [C] -> [C -> C] -> [C]
apply _ [] = []
apply l (h:t) = (apply' l h)++apply l t


actions = [a1,a2,a3,a4]
transitions = [t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12]++actions

alg' :: [C] -> [C->C] -> Int -> [C]
--alg' l r k = (alphaL (apply (gammaD l k) transitions) (k-1))++(apply l actions)++l
alg' l r k = nub((alphaL (apply (nub(gammaD l 1)) r)) k)++l

alg :: [C] -> [C->C] -> Int -> Int ->[C]
alg l r k 0 = []
--alg l r k counter = alg (alg' l r k) r k (counter-1)
alg l r k counter =
    let a = alg' l r k in
    if'
    ((length (nub a)) == length (nub (l)))
    l
    (alg a r k (counter-1))
    

test = [Null,Conf 1 1 "a" "",Conf 1 1 "" "", Conf 1 1 "" "b",Conf 1 1 "a" "b"]


kor = nub(alg test transitions 2 50)

--main = skriv(subword "abc" 2)

main = skriv( [Conf 2 3 "b" "a", Conf 2 3 "" "", Conf 2 3 "bb" "aa", Conf 4 1 "b" "a", Conf 4 1 "bb" "aa", Conf 4 1 "" ""] `intersect` kor)
--main = skriv kor
--main = skriv(sort(nub(alg test transitions 3 8)) Data.List.\\ (runProgram [(Conf 1 1 "" "")] 22))
--main = skriv (sort (nub ((runProgram [(Conf 1 1 "" "")] 10))))
c1 = Conf 1 1 "" ""
configurations = [c1]
--main = skriv (alphaL [(t1 (Conf 2 3 "" ""))] 1)

a1 (Conf 1 r c1 c2) =
    (Conf 2 r c1 c2)
a1 _ = Null

a2 (Conf 3 r c1 c2) =
    (Conf 4 r c1 c2)
a2 _ = Null

a3 (Conf s 2 c1 c2) =
    (Conf s 3 c1 c2)
a3 _ = Null

a4 (Conf s 4 c1 c2) =
    (Conf s 1 c1 c2)
a4 _ = Null



t1 (Conf 2 r c1 c2) =
    (Conf 2 r ('a':c1) c2)
t1 _ = Null

t2 (Conf 2 r c1 ('a':c2)) =
    (Conf 3 r c1 c2)
t2 _ = Null

t3 (Conf 2 r c1 ('b':c2)) =
    (Conf 2 r c1 c2)
t3 _ = Null

t4 (Conf 4 r c1 c2) =
    (Conf 4 r ('b':c1) c2)
t4 _ = Null

t5 (Conf 4 r c1 ('b':c2)) =
    (Conf 1 r c1 c2)
t5 _ = Null

t6 (Conf 4 r c1 ('a':c2)) =
    (Conf 4 r c1 c2)
t6 _ = Null





t7 (Conf s 1 c1 c2) =
    (Conf s 1 c1 ('b':c2))          
t7 _ = Null

t8 (Conf s 1 ('b':c1) c2) =
    (Conf s 1 c1 c2)
t8 _ = Null

t9 (Conf s 1 ('a':c1) c2) =
    (Conf s 2 c1 c2)
t9 _ = Null

t10 (Conf s 3 c1 c2) =
    (Conf s 3 c1 ('a':c2))   
t10 _ = Null

t11 (Conf s 3 ('a':c1) c2) =
    (Conf s 3 c1 c2)
t11 _ = Null

t12 (Conf s 3 ('b':c1) c2) =
    (Conf s 4 c1 c2)
t12 _ = Null


runProgram :: [C] -> Int -> [C]
runProgram l 0 = l
runProgram l k = runProgram (nub ((apply l transitions)++l)) (k-1)


