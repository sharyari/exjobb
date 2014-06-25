CC=ghc
RM=rm
CFLAGS2=-rtsopts -prof -auto-all 
CFLAGS=-rtsopts -prof -auto-all -O2

make: main.hs 
	$(CC) -o main main.hs $(CFLAGS)

makeunopt: main.hs 
	$(CC) -o main main.hs $(CFLAGS2)

clean: 
	$(RM) *.o