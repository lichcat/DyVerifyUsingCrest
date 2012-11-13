#inject some memory leak into replace.c with global_array.c

CREST_CFLAGS = -DCREST -I../../../include
CREST_LDFLAGS = -L../../../lib
CREST_LIBS = -lcrest -lstdc++
CREST_CC = ../../../cil/bin/cilly --save-temps --doCrestInstrument
PROGRAM = wc

ANALYZER = sourceanalyzer -b inject_$(PROGRAM)

origin:	clean
	gcc $(PROGRAM).c global_array.c -o $(PROGRAM)

FPRFILE=inject_$(PROGRAM).fpr

analysis:	clean
	$(ANALYZER) gcc global_array.c $(PROGRAM).c -o $(PROGRAM)
	$(ANALYZER) -scan -f $(FPRFILE)
	file-roller --force --extract-to=inject_fortify $(FPRFILE)
	rm -f currentCheck*
	rm -f checklists

crest:	clean
	$(CREST_CC) $(CREST_CFLAGS) -c global_array.c -o global_array.o
	$(CREST_CC) $(CREST_CFLAGS) -c $(PROGRAM).c -o $(PROGRAM).o
	$(CREST_CC) $(CREST_CFLAGS) $(PROGRAM).o global_array.o -o $(PROGRAM) $(CREST_LDFLAGS) $(CREST_LIBS)

run:
	../../../bin/reachability
	../../../bin/run_crest "./$(PROGRAM)" 1000 -path 2>$(PROGRAM)_path

log-clean:
	rm -f *.log

clean:	log-clean
	rm -f *.o *.i *.cil.c *.fpr
	rm -f -r inject_fortify
	rm -f branches cfg cfg_func_map funcount idcount stmtcount
	rm -f $(PROGRAM) input cfg_branches coverage szd_execution
