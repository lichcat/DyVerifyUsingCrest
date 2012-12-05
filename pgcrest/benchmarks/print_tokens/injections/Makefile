#inject some memory leak into replace.c with global_array.c

CREST_CFLAGS = -DCREST -I../../../include
CREST_LDFLAGS = -L../../../lib
CREST_LIBS = -lcrest -lstdc++
CREST_CC = ../../../cil/bin/cilly --save-temps --doCrestInstrument

ANALYZER = sourceanalyzer -b injec_beta_pt

origin:	clean
	gcc print_tokens.c global_array.c -o print_tokens

FPRFILE=inject_beta_pt.fpr

analysis:	clean
	$(ANALYZER) gcc global_array.c print_tokens.c -o print_tokens
	$(ANALYZER) -scan -f $(FPRFILE)
	file-roller --force --extract-to=inject_fortify $(FPRFILE)
	rm -f currentCheck*
	rm -f checklists

crest:	clean
	$(CREST_CC) $(CREST_CFLAGS) -c global_array.c -o global_array.o
	$(CREST_CC) $(CREST_CFLAGS) -c print_tokens.c -o print_tokens.o
	$(CREST_CC) $(CREST_CFLAGS) print_tokens.o global_array.o -o print_tokens $(CREST_LDFLAGS) $(CREST_LIBS)

run:
	../../../bin/reachability
	../../../bin/run_crest "./print_tokens" 1000 -path 2>pt_path

log-clean:
	rm -f *.log

clean:
	rm -f *.o *.i *.cil.c *.fpr
	rm -f -r inject_fortify
	rm -f branches cfg cfg_func_map funcount idcount stmtcount
	rm -f print_tokens input cfg_branches coverage szd_execution
	rm -f reachability
