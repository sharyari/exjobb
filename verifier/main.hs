import Verifier
import TrieModule
import Step
import ProblemFormulation
import Data.HashMap.Strict as M

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


verbose = verify (myTrie,M.empty) rules bad initial symbols 2
normal = getSize (verbose)

rules = createRuleTree (concat (Prelude.map translate transitions))
myTrie = tAdd M.empty (toConf initial)
myConf = ([toConf initial], [])
