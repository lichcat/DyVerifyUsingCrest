XML2_LIBDIR="-L/usr/local/lib"
XML2_LIBS="-lxml2"
XML2_INCLUDEDIR="-I/usr/local/include/libxml2"
CC = gcc
CFLAGS=-Wall $(XML2_LIBDIR) $(XML2_INCLUDEDIR) $(XML2_LIBS)

all:	fvdlParser

fvdlParser:	src/fvdlParser.c
	$(CC) -o fvdlParser src/fvdlParser.c $(CFLAGS)
	mv fvdlParser ./bin/fvdlParser

clean:	
	rm -f ./src/*.o
	rm -f ./bin/fvdlParser
