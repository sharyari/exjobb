#!/bin/bash
echo "Protocol & Verifier && Backward & \\\\" > report/result2.tex
echo "ABP" >> report/result2.tex
cp ProtocolsNotFromXML/ABP.hs verifier/Haskell/ProblemFormulation.hs
cp ProtocolsNotFromXML/ABP.hs verifier/Backward/ProblemFormulation.hs
ghc -o verifier/Haskell/main verifier/Haskell/main.hs -rtsopts -auto-all -O2 -threaded -iverifier/Haskell/
verifier/Haskell/main -l >> report/result2.tex
ghc -o verifier/Backward/main verifier/Backward/main.hs -rtsopts -auto-all -O2 -threaded -iverifier/Backward/
verifier/Backward/main -l >> report/result2.tex
echo "\\\\" >> report/result2.tex
echo "SW3" >> report/result2.tex
cp ProtocolsNotFromXML/SlidingWindow.hs verifier/Haskell/ProblemFormulation.hs
cp ProtocolsNotFromXML/SlidingWindow.hs verifier/Backward/ProblemFormulation.hs
ghc -o verifier/Haskell/main verifier/Haskell/main.hs -rtsopts -auto-all -O2 -threaded -iverifier/Haskell/
verifier/Haskell/main -l >> report/result2.tex
ghc -o verifier/Backward/main verifier/Backward/main.hs -rtsopts -auto-all -O2 -threaded -iverifier/Backward/
verifier/Backward/main -l >> report/result2.tex
echo "\\\\" >> report/result2.tex
echo "SW4" >> report/result2.tex
cp ProtocolsNotFromXML/SlidingWindow4.hs verifier/Haskell/ProblemFormulation.hs
cp ProtocolsNotFromXML/SlidingWindow4.hs verifier/Backward/ProblemFormulation.hs
ghc -o verifier/Haskell/main verifier/Haskell/main.hs -rtsopts -auto-all -O2 -threaded -iverifier/Haskell/
verifier/Haskell/main -l >> report/result2.tex
ghc -o verifier/Backward/main verifier/Backward/main.hs -rtsopts -auto-all -O2 -threaded -iverifier/Backward/
verifier/Backward/main -l >> report/result2.tex
echo "\\\\" >> report/result2.tex
echo "SW5" >> report/result2.tex
cp ProtocolsNotFromXML/SlidingWindow5.hs verifier/Haskell/ProblemFormulation.hs
cp ProtocolsNotFromXML/SlidingWindow5.hs verifier/Backward/ProblemFormulation.hs
ghc -o verifier/Haskell/main verifier/Haskell/main.hs -rtsopts -auto-all -O2 -threaded -iverifier/Haskell/
verifier/Haskell/main -l >> report/result2.tex
ghc -o verifier/Backward/main verifier/Backward/main.hs -rtsopts -auto-all -O2 -threaded -iverifier/Backward/
verifier/Backward/main -l >> report/result2.tex
echo "\\"

