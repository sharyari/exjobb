CC=ghc
RM=rm
CFLAGS2=-rtsopts -prof -auto-all  -threaded
CFLAGS=-rtsopts -prof -auto-all -O -threaded

make: main.hs 
	$(CC) -o main main.hs $(CFLAGS)

makeunopt: main.hs 
	$(CC) -o main main.hs $(CFLAGS2)

clean: 
	$(RM) *.o