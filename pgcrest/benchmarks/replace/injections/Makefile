#inject some memory leak into replace.c with global_array.c

CREST_CFLAGS = -DCREST -I../../../include
CREST_LDFLAGS = -L../../../lib
CREST_LIBS = -lcrest -lstdc++
CREST_CC = ../../../cil/bin/cilly --save-temps --doCrestInstrument

ANALYZER = sourceanalyzer -b injec_beta_replace

origin:	clean
	gcc replace.c global_array.c -o replace

FPRFILE=inject_beta_replace.fpr

analysis:	clean
	$(ANALYZER) gcc global_array.c replace.c -o replace
	$(ANALYZER) -scan -f $(FPRFILE)
	file-roller --force --extract-to=inject_fortify $(FPRFILE)
	rm -f currentCheck*
	rm -f checklists

crest:	clean
	$(CREST_CC) $(CREST_CFLAGS) -c global_array.c -o global_array.o
	$(CREST_CC) $(CREST_CFLAGS) -c replace.c -o replace.o
	$(CREST_CC) $(CREST_CFLAGS) replace.o global_array.o -o replace $(CREST_LDFLAGS) $(CREST_LIBS)

run:
	../../../bin/reachability
	../../../bin/run_crest "./replace" 1000 -path 2>replace_path

log-clean:
	rm -f *.log

clean:
	rm -f *.o *.i *.cil.c *.fpr
	rm -f -r inject_fortify
	rm -f branches cfg cfg_func_map funcount idcount stmtcount
	rm -f replace input cfg_branches coverage szd_execution
