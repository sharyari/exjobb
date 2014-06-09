import Control.Parallel
import Data.List
import Data.Set
import Data.Char
data C = Conf Int Int [Char] [Char] | Null    deriving (Show, Eq, Ord)
data R = Rule (C->C)


if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y
skriv a = putStrLn(show(a))



--Subwords of a set
getWordInterval a b l = take (b-a) (drop a l)

subwordK l 0 = [""]
subwordK l k = [getWordInterval x (x+k) l | x <- [0..(length l -k)]]++subwordK l (k-1)

longerwords'' Null = []
longerwords'' (Conf a b c d) = [(Conf a b c d),(Conf a b ('a':c) d),(Conf a b ('b':c) d),(Conf a b ('a':c) ('a':d)),(Conf a b ('a':c) ('b':d)), (Conf a b ('b':c) ('a':d)),(Conf a b ('b':c) ('b':d)), (Conf a b c ('a':d)),(Conf a b c ('b':d))]
longerwords l = nub (concat [longerwords'' x | x <- l])

apply l r = nub [x y | x <- r, y <- l]

gammaC l [] k = []
gammaC l (c:onf) k = 
       let a = alpha c k in
    if' 
    ((a Data.List.\\ l) == [])
    (c:gammaC l onf k)
    (gammaC l onf k)


gammaD l k = nub(gammaC l (longerwords l) (k-1))

alpha :: C -> Int -> [C]
alpha Null _ = []
alpha (Conf s r c1 c2) k =
    [Conf s r x y | x <- subwordK c1 k, y <- subwordK c2 k]

alphaL :: [C] -> Int -> [C]
alphaL [] _ = []
alphaL (h:t) k = (alpha h k)++alphaL t k


alg' :: [C] -> [C->C] -> Int -> [C]
--alg' l r k = (alphaL (apply (gammaD l k) transitions) (k-1))++(apply l actions)++l
alg' l r k = nub((alphaL (apply ((gammaD l k)) r)) k++l)


alg :: [C] -> [C->C] -> Int -> Int ->[C]
alg l r k 0 = []
alg l r k counter =
    let a = alg' l r k in
    if'
    ((length a) == length l)
    l
    (alg a r k (counter-1))
   


kor = nub(alg i transitions 5 50)
kor1 = gammaD i 2
--main = skriv( [Conf 2 3 "b" "a", Conf 2 3 "" "", Conf 2 3 "bb" "aa", Conf 4 1 "b" "a", Conf 4 1 "bb" "aa", Conf 4 1 "" ""] `intersect` kor)
main = skriv kor


--main = skriv(alpha (Conf 1 1 "" "") 1)


i = [Null, Conf 1 1 "" ""]
actions = [a1,a2,a3,a4]
transitions = [t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12]++actions

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


