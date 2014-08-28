CC=ghc
RM=rm
CFLAGS4= -rtsopts -O2 -threaded -prof -fprof-auto
CFLAGS3= -rtsopts -auto-all -O2 -threaded -eventlog
CFLAGS2= -rtsopts -auto-all
CFLAGS = -rtsopts -auto-all -O2 -threaded 
HASKELLPATH=verifier/Haskell/
SHELL=/bin/bash
CLASSPATH= verifier/Parser/xmlParser


make: $(HASKELLPATH)/main.hs 
	java -jar parser.jar ${TARGET} > $(HASKELLPATH)/ProblemFormulation.hs
	$(CC) -o $(HASKELLPATH)/main $(HASKELLPATH)/main.hs $(CFLAGS) -i$(HASKELLPATH)
	$(HASKELLPATH)/main
unopt: main.hs 
	cp ${TARGET} ProblemFormulation.hs
	$(CC) -o main main.hs $(CFLAGS2)

eventlog: main.hs
	cp ${TARGET} ProblemFormulation.hs
	$(CC) -o main main.hs $(CFLAGS3)

prof: main.hs 
	cp ${TARGET} ProblemFormulation.hs
	$(CC) -o main main.hs $(CFLAGS4)
clean: 
	$(RM) *.o
install:
	sh scripts/install.sh
