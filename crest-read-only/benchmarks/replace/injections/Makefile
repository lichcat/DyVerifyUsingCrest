#inject some memory leak into replace.c with global_array.c

CREST_CFLAGS = -DCREST -I../../../include
CREST_LDFLAGS = -L../../../lib
CREST_LIBS = -lcrest -lstdc++
CREST_CC = ../../../cil/bin/cilly --save-temps --doCrestInstrument

ANALYZER = sourceanalyzer -b injec_replace gcc

origin:	clean
	gcc replace.c global_array.c -o replace

analysis:	clean
	$(ANALYZER) -c global_array.c replace.c -o replace
	$(ANALYZER) -scan -f injec_replace.fpr
	

crest:	clean
	$(CREST_CC) $(CREST_CFLAGS) -c global_array.c -o global_array.o
	$(CREST_CC) $(CREST_CFLAGS) -c replace.c -o replace.o
	$(CREST_CC) $(CREST_CFLAGS) replace.o global_array.o -o replace $(CREST_LDFLAGS) $(CREST_LIBS)

clean:
	rm -f *.o *.i *.cil.c
	rm -f branches cfg cfg_func_map funcount idcount stmtcount
	rm -f replace
