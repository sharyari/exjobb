import Verifier

import UnOrdered
import ProblemFormulation
import Data.HashMap.Strict as M

import System.TimeIt
import System.Environment
import Control.Monad
import System.Exit
import Data.IORef

skriv a = putStrLn(show(a))

main = do
  getArgs >>=parse
  
parse ["-h"] = usage   >> exit
parse ["-l"] = latex >> exit
parse []     = skriv (normal) >> exit

usage   = putStrLn "Usage: -v verbose"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)


latex = timeIt (skriv $ verify (myTrie, M.empty) 1 False)
normal =
  "There is no message loss in the Run.hs file at the moment"
  --verify (myTrie,M.empty) 1 True
