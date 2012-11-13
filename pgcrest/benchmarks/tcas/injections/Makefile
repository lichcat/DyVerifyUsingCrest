#inject some memory leak into replace.c with global_array.c

CREST_CFLAGS = -DCREST -I../../../include
CREST_LDFLAGS = -L../../../lib
CREST_LIBS = -lcrest -lstdc++
CREST_CC = ../../../cil/bin/cilly --save-temps --doCrestInstrument
CREST_PROGRAM = tcas
ANALYZER = sourceanalyzer -b injec_$(CREST_PROGRAM)

origin:	clean
	gcc $(CREST_PROGRAM).c global_array.c -o $(CREST_PROGRAM)

FPRFILE=inject_$(CREST_PROGRAM).fpr

analysis:	clean
	$(ANALYZER) gcc global_array.c $(CREST_PROGRAM).c -o $(CREST_PROGRAM)
	$(ANALYZER) -scan -f $(FPRFILE)
	file-roller --force --extract-to=inject_fortify $(FPRFILE)
	rm -f currentCheck*
	rm -f checklists

crest:	clean
	$(CREST_CC) $(CREST_CFLAGS) -c global_array.c -o global_array.o
	$(CREST_CC) $(CREST_CFLAGS) -c $(CREST_PROGRAM).c -o $(CREST_PROGRAM).o
	$(CREST_CC) $(CREST_CFLAGS) $(CREST_PROGRAM).o global_array.o -o $(CREST_PROGRAM) $(CREST_LDFLAGS) $(CREST_LIBS)

run:
	../../../bin/reachability
	../../../bin/run_crest "./$(CREST_PROGRAM)" 1000 -path 2>$(CREST_PROGRAM)_path

distinct-clean:
	rm -f *.log
	rm -f currentCheck* checklists
	rm -f -r inject_fortify
	rm -f *.fpr

clean:
	rm -f *.o *.i *.cil.c
	rm -f branches cfg cfg_func_map funcount idcount stmtcount
	rm -f $(CREST_PROGRAM) input cfg_branches coverage szd_execution
