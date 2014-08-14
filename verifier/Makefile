CC=ghc
RM=rm
CFLAGS4= -rtsopts -O2 -threaded -prof -fprof-auto
CFLAGS3= -rtsopts -auto-all -O2 -threaded -eventlog
CFLAGS2= -rtsopts -auto-all
CFLAGS = -rtsopts -auto-all -O2 -threaded 

make: main.hs 
	cp ${TARGET} ProblemFormulation.hs
	$(CC) -o main main.hs $(CFLAGS)

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