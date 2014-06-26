import DataTypes
import Verifier
import TrieModule
import StringManipulation
import Gamma
import Step
import Alpha
--import Run

import Data.List as L
import Data.Trie as T
import Data.ByteString as B
import Prelude as P
import Data.Set as S
import qualified Data.ByteString.Char8 as B2


import System.Environment
import System.Exit

--import ABP
import SlidingWindow
--set 5: 3.2 6: 7.6, 7: 16.4
--SeenTrie : 5: 0.3 6: 0.45 7: 0.73
--ByteString 5: 0.4 6: 0.71 7: 1.12

main = getArgs >>= parse >>= P.putStr . tac
tac  = P.unlines . P.reverse . P.lines

parse ["-h"] = usage   >> exit
parse ["-v"] = skriv (verify (myTrie,T.empty) rules 2 60) >> exit
--parse ["-r"] = skriv (S.size (run myConf rules 2)) >> exit
parse []     = skriv (getSize (verify (myTrie,T.empty) rules 2 200)) >> exit
--parse ["-t"] = skriv (nlonger (L.map B2.pack ["ab", "cd"]) 2) >> exit
parse fs     = P.concat `P.fmap` P.mapM P.readFile fs

usage   = P.putStrLn "Usage: -v verbose"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)


rules = createRuleTree (L.concat (L.map translate transitions))

myTrie = tAdd T.empty (toConf initial)
myConf = ([toConf initial], [])

--test1 = alpha(step(gamma(myTrie)))
--test2 = alpha(step(gamma test1))
