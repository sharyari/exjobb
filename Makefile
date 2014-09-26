CFLAGS4= -rtsopts -O2 -threaded -prof -fprof-auto
CFLAGS3= -rtsopts -auto-all -O2 -threaded -eventlog
CFLAGS2= -rtsopts -auto-all
CFLAGS = -rtsopts -auto-all -O2 -threaded 
HASKELLPATH=verifier/Haskell/
BACKWARDPATH=verifier/Backward/
SHELL=/bin/bash
CLASSPATH= verifier/Parser/xmlParser
CC=ghc
RM=rm

make: $(HASKELLPATH)/main.hs 
	java -jar parser.jar ${r} > $(HASKELLPATH)/ProblemFormulation.hs
	$(CC) -o $(HASKELLPATH)/main $(HASKELLPATH)/main.hs $(CFLAGS) -i$(HASKELLPATH)
	time $(HASKELLPATH)/main
unopt: $(HASKELLPATH)/main.hs 
	java -jar parser.jar ${r} > $(HASKELLPATH)/ProblemFormulation.hs
	$(CC) -o $(HASKELLPATH)/main $(HASKELLPATH)/main.hs $(CFLAGS) -i$(HASKELLPATH)
	$(HASKELLPATH)/main
eventlog: main.hs
	java -jar parser.jar ${r} > $(HASKELLPATH)/ProblemFormulation.hs
	$(CC) -o $(HASKELLPATH)/main $(HASKELLPATH)/main.hs $(CFLAGS) -i$(HASKELLPATH)
	$(HASKELLPATH)/main
prof: $(HASKELLPATH)/main.hs 
	java -jar parser.jar ${r} > $(HASKELLPATH)/ProblemFormulation.hs
	$(CC) -o $(HASKELLPATH)/main $(HASKELLPATH)/main.hs $(CFLAGS4) -i$(HASKELLPATH)
	$(HASKELLPATH)/main

stat: $(HASKELLPATH)/main.hs 
	java -jar parser.jar ${r} > $(HASKELLPATH)/ProblemFormulation.hs
	$(CC) -o $(HASKELLPATH)/main $(HASKELLPATH)/main.hs $(CFLAGS) -i$(HASKELLPATH)
	time $(HASKELLPATH)/main -l >> report/result1.tex
	java -jar parser.jar ${r} > $(BACKWARDPATH)/ProblemFormulation.hs
	$(CC) -o $(BACKWARDPATH)/main $(BACKWARDPATH)/main.hs $(CFLAGS) -i$(BACKWARDPATH)
	time $(BACKWARDPATH)/main -l >> report/result1.tex


clean: 
	$(RM) $(HASKELLPATH)/*.o

install:
	sh scripts/install.sh

test:
	sh scripts/tests.sh
