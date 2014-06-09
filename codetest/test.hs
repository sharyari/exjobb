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
subwordK l k = nub [getWordInterval x (x+k) l | x <- [0..(length l -k)]]++subwordK l (k-1)

longerwords'' Null = []
longerwords'' (Conf a b c d) = [(Conf a b c d), (Conf a b ('a':c) d),(Conf a b ('b':c) d),(Conf a b ('a':c) ('a':d)),(Conf a b ('a':c) ('b':d)), (Conf a b ('b':c) ('a':d)),(Conf a b ('b':c) ('b':d)), (Conf a b c ('a':d)),(Conf a b c ('b':d))]
longerwords l = nub (concat [longerwords'' x | x <- l])

apply l r = nub [x y | x <- r, y <- l]


gammaC l [] k = []
gammaC l (c:onf) k = 
       let a = alpha c k in
    if' 
    ((nub(a) Data.List.\\ l) == [])
    (c:gammaC l onf k)
    (gammaC l onf k)


gammaD l k = nub(gammaC l (longerwords l) (k-1))

alpha :: C -> Int -> [C]
alpha Null _ = []
alpha (Conf s r c1 c2) k =
    [Conf s r x y | x <- subwordK c1 k, y <- subwordK c2 k]

alphaL :: [C] -> Int -> [C]
alphaL l k = concat[alpha x k | x <-l]

alg' :: [C] -> [C->C] -> Int -> [C]
alg' l r k = nub((alphaL (apply ((gammaD l k)) r)) k++l)


alg :: [C] -> [C->C] -> Int -> Int ->[C]
alg l r k 0 = []
alg l r k counter =
    let a = alg' l r k in
    if'
    ((length a) == length l)
    l
    (alg a r k (counter-1))
   


kor = nub(alg i transitions 2 15)
kor1 = gammaD i 2
--main = skriv( [Conf 2 3 "a" "b", Conf 2 3 "" "", Conf 2 3 "aa" "bb", Conf 4 1 "b" "a", Conf 4 1 "bb" "aa", Conf 4 1 "" ""] `intersect` kor)
main = skriv (sort kor)
--main = skriv(gammaC test [Conf 2 1 "aa" ""] 2)
--main = skriv(nub(alpha (Conf 2 1 "aa" "") 2))
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


test = [Conf 1 1 "" "",Conf 1 1 "" "a",Conf 1 1 "" "ab",Conf 1 1 "" "b",Conf 1 1 "" "ba",Conf 1 1 "" "bab",Conf 1 1 "" "bb",Conf 1 1 "" "bba",Conf 1 1 "a" "",Conf 1 1 "a" "a",Conf 1 1 "a" "b",Conf 1 1 "a" "ba",Conf 1 1 "a" "bb",Conf 1 1 "a" "bba",Conf 1 1 "ab" "",Conf 1 1 "ab" "b",Conf 1 1 "ab" "bb",Conf 1 1 "b" "",Conf 1 1 "b" "a",Conf 1 1 "b" "b",Conf 1 1 "b" "ba",Conf 1 1 "b" "bb",Conf 1 1 "b" "bba",Conf 1 1 "ba" "",Conf 1 1 "ba" "a",Conf 1 1 "ba" "b",Conf 1 1 "ba" "ba",Conf 1 1 "ba" "bb",Conf 1 1 "ba" "bba",Conf 1 2 "" "",Conf 1 2 "" "a",Conf 1 2 "" "b",Conf 1 2 "" "ba",Conf 1 2 "a" "",Conf 1 2 "b" "",Conf 1 2 "b" "a",Conf 1 2 "b" "b",Conf 1 2 "ba" "",Conf 1 3 "" "",Conf 1 3 "" "a",Conf 1 3 "" "aa",Conf 1 3 "" "aab",Conf 1 3 "" "ab",Conf 1 3 "" "aba",Conf 1 3 "" "b",Conf 1 3 "" "ba",Conf 1 3 "a" "",Conf 1 3 "a" "a",Conf 1 3 "a" "aa",Conf 1 3 "ab" "",Conf 1 3 "ab" "a",Conf 1 3 "ab" "aa",Conf 1 3 "b" "",Conf 1 3 "b" "a",Conf 1 3 "b" "aa",Conf 1 3 "b" "aab",Conf 1 3 "b" "ab",Conf 1 3 "b" "b",Conf 1 3 "ba" "",Conf 1 3 "ba" "a",Conf 1 3 "ba" "aa",Conf 1 4 "" "",Conf 1 4 "" "a",Conf 1 4 "" "ab",Conf 1 4 "" "b",Conf 1 4 "a" "",Conf 1 4 "a" "a",Conf 1 4 "b" "",Conf 1 4 "b" "a",Conf 1 4 "ba" "",Conf 2 1 "" "",Conf 2 1 "" "a",Conf 2 1 "" "ab",Conf 2 1 "" "b",Conf 2 1 "" "ba",Conf 2 1 "" "bab",Conf 2 1 "" "bb",Conf 2 1 "" "bba",Conf 2 1 "a" "",Conf 2 1 "a" "a",Conf 2 1 "a" "ab",Conf 2 1 "a" "b",Conf 2 1 "a" "ba",Conf 2 1 "a" "bab",Conf 2 1 "a" "bb",Conf 2 1 "a" "bba",Conf 2 1 "aa" "",Conf 2 1 "aa" "a",Conf 2 1 "aa" "ab",Conf 2 1 "aa" "b",Conf 2 1 "aa" "ba",Conf 2 1 "aab" "",Conf 2 1 "aab" "a",Conf 2 1 "aab" "b",Conf 2 1 "aab" "ba",Conf 2 1 "ab" "",Conf 2 1 "ab" "a",Conf 2 1 "ab" "b",Conf 2 1 "ab" "ba",Conf 2 1 "ab" "bb",Conf 2 1 "ab" "bba",Conf 2 1 "aba" "",Conf 2 1 "aba" "a",Conf 2 1 "aba" "b",Conf 2 1 "aba" "ba",Conf 2 1 "b" "",Conf 2 1 "b" "a",Conf 2 1 "b" "b",Conf 2 1 "b" "ba",Conf 2 1 "b" "bb",Conf 2 1 "b" "bba",Conf 2 1 "ba" "",Conf 2 1 "ba" "a",Conf 2 1 "ba" "b",Conf 2 1 "ba" "ba",Conf 2 1 "ba" "bb",Conf 2 1 "ba" "bba",Conf 2 2 "" "",Conf 2 2 "" "a",Conf 2 2 "" "ab",Conf 2 2 "" "b",Conf 2 2 "" "ba",Conf 2 2 "a" "",Conf 2 2 "a" "a",Conf 2 2 "a" "ab",Conf 2 2 "a" "b",Conf 2 2 "a" "ba",Conf 2 2 "aa" "",Conf 2 2 "aa" "a",Conf 2 2 "aa" "ab",Conf 2 2 "aa" "b",Conf 2 2 "aa" "ba",Conf 2 2 "aab" "",Conf 2 2 "aab" "a",Conf 2 2 "aab" "b",Conf 2 2 "aab" "ba",Conf 2 2 "ab" "",Conf 2 2 "ab" "a",Conf 2 2 "ab" "b",Conf 2 2 "ab" "ba",Conf 2 2 "aba" "",Conf 2 2 "b" "",Conf 2 2 "b" "a",Conf 2 2 "b" "b",Conf 2 2 "b" "ba",Conf 2 2 "ba" "",Conf 2 3 "" "",Conf 2 3 "" "a",Conf 2 3 "" "aa",Conf 2 3 "" "aab",Conf 2 3 "" "ab",Conf 2 3 "" "aba",Conf 2 3 "" "b",Conf 2 3 "" "ba",Conf 2 3 "a" "",Conf 2 3 "a" "a",Conf 2 3 "a" "aa",Conf 2 3 "a" "aab",Conf 2 3 "a" "ab",Conf 2 3 "a" "aba",Conf 2 3 "a" "b",Conf 2 3 "a" "ba",Conf 2 3 "aa" "",Conf 2 3 "aa" "a",Conf 2 3 "aa" "ab",Conf 2 3 "aa" "b",Conf 2 3 "aa" "ba",Conf 2 3 "aab" "",Conf 2 3 "aab" "a",Conf 2 3 "aab" "ab",Conf 2 3 "aab" "b",Conf 2 3 "aab" "ba",Conf 2 3 "ab" "",Conf 2 3 "ab" "a",Conf 2 3 "ab" "aa",Conf 2 3 "ab" "aab",Conf 2 3 "ab" "ab",Conf 2 3 "ab" "aba",Conf 2 3 "ab" "b",Conf 2 3 "ab" "ba",Conf 2 3 "aba" "",Conf 2 3 "aba" "a",Conf 2 3 "b" "",Conf 2 3 "b" "a",Conf 2 3 "b" "aa",Conf 2 3 "b" "aab",Conf 2 3 "b" "ab",Conf 2 3 "b" "aba",Conf 2 3 "b" "b",Conf 2 3 "b" "ba",Conf 2 3 "ba" "",Conf 2 3 "ba" "a",Conf 2 3 "ba" "aa",Conf 2 4 "" "",Conf 2 4 "" "a",Conf 2 4 "" "ab",Conf 2 4 "" "b",Conf 2 4 "" "ba",Conf 2 4 "a" "",Conf 2 4 "a" "a",Conf 2 4 "a" "ab",Conf 2 4 "a" "b",Conf 2 4 "a" "ba",Conf 2 4 "aa" "",Conf 2 4 "aa" "a",Conf 2 4 "aa" "ab",Conf 2 4 "aa" "b",Conf 2 4 "aa" "ba",Conf 2 4 "aab" "",Conf 2 4 "aab" "a",Conf 2 4 "ab" "",Conf 2 4 "ab" "a",Conf 2 4 "aba" "",Conf 2 4 "b" "",Conf 2 4 "b" "a",Conf 2 4 "ba" "",Conf 3 1 "" "",Conf 3 1 "" "a",Conf 3 1 "" "ab",Conf 3 1 "" "b",Conf 3 1 "" "ba",Conf 3 1 "" "bab",Conf 3 1 "" "bb",Conf 3 1 "" "bba",Conf 3 1 "a" "",Conf 3 1 "a" "a",Conf 3 1 "a" "b",Conf 3 1 "a" "ba",Conf 3 1 "a" "bb",Conf 3 1 "a" "bba",Conf 3 1 "ab" "",Conf 3 1 "ab" "b",Conf 3 1 "ab" "bb",Conf 3 1 "b" "",Conf 3 1 "b" "b",Conf 3 1 "b" "bb",Conf 3 1 "ba" "",Conf 3 1 "ba" "b",Conf 3 1 "ba" "bb",Conf 3 2 "" "",Conf 3 2 "" "a",Conf 3 2 "" "b",Conf 3 2 "" "ba",Conf 3 2 "a" "",Conf 3 2 "a" "b",Conf 3 2 "ab" "",Conf 3 2 "b" "",Conf 3 2 "b" "b",Conf 3 3 "" "",Conf 3 3 "" "a",Conf 3 3 "" "aa",Conf 3 3 "" "aab",Conf 3 3 "" "ab",Conf 3 3 "" "aba",Conf 3 3 "" "b",Conf 3 3 "" "ba",Conf 3 3 "a" "",Conf 3 3 "a" "a",Conf 3 3 "a" "aa",Conf 3 3 "a" "aab",Conf 3 3 "a" "ab",Conf 3 3 "a" "b",Conf 3 3 "ab" "",Conf 3 3 "ab" "a",Conf 3 3 "ab" "aa",Conf 3 3 "ab" "aab",Conf 3 3 "ab" "ab",Conf 3 3 "ab" "b",Conf 3 3 "b" "",Conf 3 3 "b" "a",Conf 3 3 "b" "aa",Conf 3 3 "b" "aab",Conf 3 3 "b" "ab",Conf 3 3 "b" "b",Conf 3 3 "ba" "",Conf 3 3 "ba" "a",Conf 3 3 "ba" "aa",Conf 3 4 "" "",Conf 3 4 "" "a",Conf 3 4 "" "ab",Conf 3 4 "" "b",Conf 3 4 "a" "",Conf 3 4 "a" "a",Conf 3 4 "a" "b",Conf 3 4 "ab" "",Conf 3 4 "b" "",Conf 4 1 "" "",Conf 4 1 "" "a",Conf 4 1 "" "ab",Conf 4 1 "" "b",Conf 4 1 "" "ba",Conf 4 1 "" "bab",Conf 4 1 "" "bb",Conf 4 1 "" "bba",Conf 4 1 "a" "",Conf 4 1 "a" "a",Conf 4 1 "a" "ab",Conf 4 1 "a" "b",Conf 4 1 "a" "ba",Conf 4 1 "a" "bab",Conf 4 1 "a" "bb",Conf 4 1 "a" "bba",Conf 4 1 "ab" "",Conf 4 1 "ab" "b",Conf 4 1 "ab" "bb",Conf 4 1 "b" "",Conf 4 1 "b" "a",Conf 4 1 "b" "ab",Conf 4 1 "b" "b",Conf 4 1 "b" "ba",Conf 4 1 "b" "bab",Conf 4 1 "b" "bb",Conf 4 1 "b" "bba",Conf 4 1 "ba" "",Conf 4 1 "ba" "a",Conf 4 1 "ba" "ab",Conf 4 1 "ba" "b",Conf 4 1 "ba" "ba",Conf 4 1 "ba" "bab",Conf 4 1 "ba" "bb",Conf 4 1 "ba" "bba",Conf 4 1 "bab" "",Conf 4 1 "bab" "b",Conf 4 1 "bb" "",Conf 4 1 "bb" "a",Conf 4 1 "bb" "ab",Conf 4 1 "bb" "b",Conf 4 1 "bb" "ba",Conf 4 1 "bba" "",Conf 4 1 "bba" "a",Conf 4 1 "bba" "ab",Conf 4 1 "bba" "b",Conf 4 1 "bba" "ba",Conf 4 2 "" "",Conf 4 2 "" "a",Conf 4 2 "" "ab",Conf 4 2 "" "b",Conf 4 2 "" "ba",Conf 4 2 "a" "",Conf 4 2 "a" "b",Conf 4 2 "ab" "",Conf 4 2 "b" "",Conf 4 2 "b" "a",Conf 4 2 "b" "ab",Conf 4 2 "b" "b",Conf 4 2 "b" "ba",Conf 4 2 "ba" "",Conf 4 2 "ba" "b",Conf 4 2 "bab" "",Conf 4 2 "bb" "",Conf 4 2 "bb" "a",Conf 4 2 "bb" "ab",Conf 4 2 "bb" "b",Conf 4 2 "bb" "ba",Conf 4 2 "bba" "",Conf 4 2 "bba" "b",Conf 4 3 "" "",Conf 4 3 "" "a",Conf 4 3 "" "aa",Conf 4 3 "" "aab",Conf 4 3 "" "ab",Conf 4 3 "" "aba",Conf 4 3 "" "b",Conf 4 3 "" "ba",Conf 4 3 "a" "",Conf 4 3 "a" "a",Conf 4 3 "a" "aa",Conf 4 3 "a" "aab",Conf 4 3 "a" "ab",Conf 4 3 "a" "b",Conf 4 3 "ab" "",Conf 4 3 "ab" "a",Conf 4 3 "ab" "aa",Conf 4 3 "ab" "aab",Conf 4 3 "ab" "ab",Conf 4 3 "ab" "b",Conf 4 3 "b" "",Conf 4 3 "b" "a",Conf 4 3 "b" "aa",Conf 4 3 "b" "aab",Conf 4 3 "b" "ab",Conf 4 3 "b" "aba",Conf 4 3 "b" "b",Conf 4 3 "b" "ba",Conf 4 3 "ba" "",Conf 4 3 "ba" "a",Conf 4 3 "ba" "aa",Conf 4 3 "ba" "aab",Conf 4 3 "ba" "ab",Conf 4 3 "ba" "b",Conf 4 3 "bab" "",Conf 4 3 "bab" "a",Conf 4 3 "bab" "ab",Conf 4 3 "bab" "b",Conf 4 3 "bb" "",Conf 4 3 "bb" "a",Conf 4 3 "bb" "ab",Conf 4 3 "bb" "b",Conf 4 3 "bb" "ba",Conf 4 3 "bba" "",Conf 4 3 "bba" "a",Conf 4 3 "bba" "ab",Conf 4 3 "bba" "b",Conf 4 4 "" "",Conf 4 4 "" "a",Conf 4 4 "" "ab",Conf 4 4 "" "b",Conf 4 4 "" "ba",Conf 4 4 "a" "",Conf 4 4 "a" "a",Conf 4 4 "a" "ab",Conf 4 4 "a" "b",Conf 4 4 "ab" "",Conf 4 4 "b" "",Conf 4 4 "b" "a",Conf 4 4 "b" "ab",Conf 4 4 "b" "b",Conf 4 4 "b" "ba",Conf 4 4 "ba" "",Conf 4 4 "ba" "a",Conf 4 4 "ba" "ab",Conf 4 4 "ba" "b",Conf 4 4 "bab" "",Conf 4 4 "bb" "",Conf 4 4 "bb" "a",Conf 4 4 "bb" "ab",Conf 4 4 "bb" "b",Conf 4 4 "bb" "ba",Conf 4 4 "bba" "",Conf 4 4 "bba" "a",Conf 4 4 "bba" "ab",Conf 4 4 "bba" "b",Null]
