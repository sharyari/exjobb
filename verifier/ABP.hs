module ProblemFormulation where
import Data.List as L
import Data.Word
import qualified Data.ByteString.Char8 as B2

symbols = L.map B2.pack ["a","b"]


initial :: [Word8]
initial = [1,1,1]

s1 = ([(0,2,2)], (0,"!","a"))
s2 = ([(0,2,2)], (1,"?","b"))
s3 = ([(0,2,3)], (1,"?","a"))
s5 = ([(0,4,4)], (0,"!","b"))
s6 = ([(0,4,4)], (1,"?","a"))
s7 = ([(0,4,1)], (1,"?","b"))
r0 = ([(1,1,2)], (0,"?","a"))
r1 = ([(1,1,1)], (0,"?","b"))
r2 = ([(1,1,1)], (1,"!","b"))
r4 = ([(1,3,4)], (0,"?","b"))
r5 = ([(1,3,3)], (0,"?","a"))
r6 = ([(1,3,3)], (1,"!","a"))
sync0 = ([(2,1,2),(0,1,2)], (1,"_","b"))
sync1 = ([(2,1,2),(0,3,4)], (1,"_","b"))
sync2 = ([(2,2,3),(0,1,2)], (1,"_","b"))
sync3 = ([(2,2,3),(0,3,4)], (1,"_","b"))
sync4 = ([(2,2,1),(1,2,3)], (1,"_","b"))
sync5 = ([(2,2,1),(1,4,1)], (1,"_","b"))
sync6 = ([(2,1,3),(1,2,3)], (1,"_","b"))
sync7 = ([(2,1,3),(1,4,1)], (1,"_","b"))

transitions :: [([(Int,Int,Int)], (Int,String,String))]
transitions = [s1,s2,s3,s5,s6,s7,r0,r1,r2,r4,r5,r6,sync0,sync1,sync2,sync3,sync4,sync5,sync6,sync7]

