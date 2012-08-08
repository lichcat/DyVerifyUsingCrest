SCA=sourceanalyzer -b multitest gcc
CC=../../cil/bin/cilly 
LDFLAGS=--merge --keepmerged 
CFLAGS=--merge -g --save-temps --doCrestInstrument \
	   -I. -I../../include -L../../lib -lcrest -lstdc++

all:
	make clean
	$(CC) -c foo.c -o foo.o $(CFLAGS)
	$(CC) -c path_concolic.c -o path_concolic.o $(CFLAGS)
	$(CC) $(LDFLAGS) foo.o path_concolic.o -o ./path_concolic $(CFLAGS)
	../../bin/process_cfg

run:
	../../bin/run_crest ./path_concolic 1000 -path_guide

clean:
	rm -f branches cfg cfg_func_map
	rm -f funcount idcount stmtcount
	rm -f *.o *.cil.c *.i
