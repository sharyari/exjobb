import DataTypes
import Verifier
import TrieModule
import StringManipulation
import Gamma
import Step
import Alpha


import Data.List as L
import Data.Trie as T




import System.Environment
import System.Exit

--import ABP
import SlidingWindow


main = getArgs >>= parse >>= putStr . tac
tac  = unlines . reverse . lines

parse ["-h"] = usage   >> exit
parse ["-v"] = skriv (verify myTrie rules 2 60) >> exit
parse []     = skriv (getSize (verify myTrie rules 2 60)) >> exit
parse fs     = concat `fmap` mapM readFile fs

usage   = putStrLn "Usage: -v verbose"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)


rules = createRuleTree (L.concat (L.map translate transitions))

myTrie = tAdd T.empty (toConf initial) False

--test1 = alpha(step(gamma(myTrie)))
--test2 = alpha(step(gamma test1))
