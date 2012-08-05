SCA=sourceanalyzer -b multitest gcc
CC=../../cil/bin/cilly 
LDFLAGS=--merge --keepmerged 
CFLAGS=--merge -g --save-temps --doCrestInstrument \
	   -I. -I../../include -L../../lib -lcrest -lstdc++

all:
	$(CC) -c foo.c -o foo.o $(CFLAGS)
	$(CC) -c path_concolic.c -o path_concolic.o $(CFLAGS)
	$(CC) $(LDFLAGS) foo.o path_concolic.o -o ./path_concolic $(CFLAGS)

clean:
	rm branches cfg cfg_func_map
	rm funcount idcount stmtcount
	rm *.o *.cil.c *.i
