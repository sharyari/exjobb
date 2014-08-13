import Verifier
import TrieModule
import UnOrdered
import ProblemFormulation
import Data.HashMap.Strict as M

import System.Environment
import Control.Monad
import System.Exit


skriv a = putStrLn(show(a))

main = do
  getArgs >>=parse

parse ["-h"] = usage   >> exit
parse ["-v"] = skriv (verbose) >> exit
parse []     = skriv (normal) >> exit

usage   = putStrLn "Usage: -v verbose"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)


verbose = verify (myTrie,M.empty) rules bad initial symbols 2
normal =
  getSize (verbose)
