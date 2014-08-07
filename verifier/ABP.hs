module ProblemFormulation where
import Data.List as L
import Data.Word
import DataTypes
import qualified Data.ByteString.Char8 as B2

symbols :: Symbols
symbols = [[0,1],[0,1]]


initial :: [Word8]
initial = [1,1,1]

s1 = ([(0,2,2)], (0,"!",[0]))
s2 = ([(0,2,2)], (1,"?",[1]))
s3 = ([(0,2,3)], (1,"?",[0]))
s5 = ([(0,4,4)], (0,"!",[1]))
s6 = ([(0,4,4)], (1,"?",[0]))
s7 = ([(0,4,1)], (1,"?",[1]))
r0 = ([(1,1,2)], (0,"?",[0]))
r1 = ([(1,1,1)], (0,"?",[1]))
r2 = ([(1,1,1)], (1,"!",[1]))
r4 = ([(1,3,4)], (0,"?",[1]))
r5 = ([(1,3,3)], (0,"?",[0]))
r6 = ([(1,3,3)], (1,"!",[0]))
sync0 = ([(2,1,2),(0,1,2)], (1,"_",[1]))
sync1 = ([(2,1,2),(0,3,4)], (1,"_",[1]))
sync2 = ([(2,2,3),(0,1,2)], (1,"_",[1]))
sync3 = ([(2,2,3),(0,3,4)], (1,"_",[1]))
sync4 = ([(2,2,1),(1,2,3)], (1,"_",[1]))
sync5 = ([(2,2,1),(1,4,1)], (1,"_",[1]))
sync6 = ([(2,1,3),(1,2,3)], (1,"_",[1]))
sync7 = ([(2,1,3),(1,4,1)], (1,"_",[1]))

transitions :: [([(Int,Int,Int)], (Int,String,[Int]))]
transitions = [s1,s2,s3,s5,s6,s7,r0,r1,r2,r4,r5,r6,sync0,sync1,sync2,sync3,sync4,sync5,sync6,sync7]


numStates1 :: Int
numStates1 = 4
numStates2 :: Int
numStates2 = 4
numStates3 :: Int
numStates3 = 3

bad :: [(Int, Word8)]
bad = [(2,3)]