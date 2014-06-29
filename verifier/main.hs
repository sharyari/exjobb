import Verifier
import TrieModule
import Step
import ProblemFormulation

import System.Environment
import Control.Monad
import System.Exit
import Data.Trie as T


skriv a = putStrLn(show(a))

main = getArgs >>=parse

parse ["-h"] = usage   >> exit
parse ["-v"] = skriv (verbose) >> exit
parse []     = skriv (normal) >> exit

usage   = putStrLn "Usage: -v verbose"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)


verbose = verify (myTrie,T.empty) rules initial symbols 1
normal = getSize (verbose)

rules = createRuleTree (concat (map translate transitions))
myTrie = tAdd T.empty (toConf initial)
myConf = ([toConf initial], [])

--test1 = alpha(step(gamma(myTrie)))
--test2 = alpha(step(gamma test1))
