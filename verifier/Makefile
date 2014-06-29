CC=ghc
RM=rm
CFLAGS4= -rtsopts -auto-all -O -threaded -prof
CFLAGS3= -rtsopts -auto-all -O -threaded -eventlog
CFLAGS2= -rtsopts -auto-all
CFLAGS = -rtsopts -auto-all -O -threaded

make: main.hs 
	$(CC) -o main main.hs $(CFLAGS)

makeunopt: main.hs 
	$(CC) -o main main.hs $(CFLAGS2)

eventlog: main.hs 
	$(CC) -o main main.hs $(CFLAGS3)

prof: main.hs 
	$(CC) -o main main.hs $(CFLAGS4)
clean: 
	$(RM) *.o