import Control.Parallel
import Data.List
import Data.Set
import Data.Char
data C = Conf Int Int [Char] [Char] | Null    deriving (Show, Eq, Ord)
data R = Rule (C->C)
-- 5: 12.4, 6: 36.9, 7: 106
-- 5: 3.2 6: 7.6, 7: 16.4
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y
skriv a = putStrLn(show(a))

--Subwords of a set
getWordInterval a b l = take (b-a) (drop a l)

subwordK l 0 = [""]
subwordK l k = nub [getWordInterval x (x+k) l | x <- [0..(length l -k)]]++subwordK l (k-1)


symbols :: [Char]
symbols = ['a','b']

longerwords'' :: C -> Set C
--longerwords'' Null = empty
longerwords'' (Conf a b c d) = fromList [Conf a b c d, Conf a b ('a':c) ('a':d),Conf a b ('a':c) ('b':d), Conf a b ('b':c) ('a':d),Conf a b ('b':c) ('b':d)]


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
alpha k (Conf s r c1 c2) =
    fromList [Conf s r x y | x <- subwordK c1 k, y <- subwordK c2 k]

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


kor = alg (fromList initial) transitions 7 50
main = skriv (size kor)


s0 (Conf 1 r m a ) =
	(Conf 2 r m a )
s0 _ = Null


s1 (Conf 2 r m a ) =
	(Conf 2 r (m++['a']) a )
s1 _ = Null


s2 (Conf 2 r m ('b':a )) =
	(Conf 2 r m a )
s2 _ = Null


s3 (Conf 2 r m ('a':a )) =
	(Conf 3 r m a )
s3 _ = Null


s4 (Conf 3 r m a ) =
	(Conf 4 r m a )
s4 _ = Null


s5 (Conf 4 r m a ) =
	(Conf 4 r (m++['b']) a )
s5 _ = Null


s6 (Conf 4 r m ('a':a )) =
	(Conf 4 r m a )
s6 _ = Null


s7 (Conf 4 r m ('b':a )) =
	(Conf 1 r m a )
s7 _ = Null


r0 (Conf s 1 ('a':m) a ) =
	(Conf s 2 m a )
r0 _ = Null


r1 (Conf s 1 ('b':m) a ) =
	(Conf s 1 m a )
r1 _ = Null


r2 (Conf s 1 m a ) =
	(Conf s 1 m (a ++['b']))
r2 _ = Null


r3 (Conf s 2 m a ) =
	(Conf s 3 m a )
r3 _ = Null


r4 (Conf s 3 ('b':m) a ) =
	(Conf s 4 m a )
r4 _ = Null


r5 (Conf s 3 ('a':m) a ) =
	(Conf s 3 m a )
r5 _ = Null


r6 (Conf s 3 m a ) =
	(Conf s 3 m (a ++['a']))
r6 _ = Null


r7 (Conf s 4 m a ) =
	(Conf s 1 m a )
r7 _ = Null


transitions = [s0,s1,s2,s3,s4,s5,s6,s7,r0,r1,r2,r3,r4,r5,r6,r7]

initial = [(Conf 1 1 "" "" )]
