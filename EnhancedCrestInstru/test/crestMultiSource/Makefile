SCA=sourceanalyzer -b multitest gcc
CC=../../cil/bin/cilly --merge --keepmerged 
CFLAGS=--merge -g --save-temps --doCrestInstrument \
	   -I. -I../../include -L../../lib -lcrest -lstdc++
analysis:	
	$(SCA) -c foo.c -o foo.o 
	$(SCA) -c path_concolic.c -o path_concolic.o 
	$(SCA) foo.o path_concolic.o -o ./path_concolic 
all:
	$(CC) -c foo.c -o foo.o $(CFLAGS)
	$(CC) -c path_concolic.c -o path_concolic.o $(CFLAGS)
	$(CC) foo.o path_concolic.o -o ./path_concolic $(CFLAGS)
