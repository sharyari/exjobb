import DataTypes
import Verifier
import TrieModule
import StringManipulation
import Gamma
import Step
import Alpha

import Data.Trie as T

myTrie = tAdd T.empty initial False

test1 = alpha(step(gamma(myTrie)))
test2 = alpha(step(gamma test1))

main = skriv (verify myTrie 1)


s1 = ([(1,2,2)], [(1,"!","a")])
s2 = ([(1,2,2)], [(2,"?","b")])
s3 = ([(1,2,3)], [(2,"?","a")])
s5 = ([(1,4,4)], [(1,"!","b")])
s6 = ([(1,4,4)], [(2,"?","a")])
s7 = ([(1,4,1)], [(2,"?","b")])
r0 = ([(2,1,2)], [(1,"?","a")])
r1 = ([(2,1,1)], [(1,"?","b")])
r2 = ([(2,1,1)], [(2,"!","b")])
r4 = ([(2,3,4)], [(1,"?","b")])
r5 = ([(2,3,3)], [(1,"?","a")])
r6 = ([(2,3,3)], [(2,"!","a")])
sync0 = ([(3,1,2),(1,1,2)], [])
sync1 = ([(3,1,2),(1,3,4)], [])
sync2 = ([(3,2,3),(1,1,2)], [])
sync3 = ([(3,2,3),(1,3,4)], [])
sync4 = ([(3,2,1),(2,2,3)], [])
sync5 = ([(3,2,1),(2,4,1)], [])
sync6 = ([(3,1,3),(2,2,3)], [])
sync7 = ([(3,1,3),(2,4,1)], [])

transitions = [s1,s2,s3,s5,s6,s7,r0,r1,r2,r4,r5,r6,sync0,sync1,sync2,sync3,sync4,sync5,sync6,sync7]

