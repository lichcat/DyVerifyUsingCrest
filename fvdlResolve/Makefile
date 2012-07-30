XML2_LIBDIR="-L/usr/lib"
XML2_LIBS="-lxml2"
XML2_INCLUDEDIR="-I/usr/include/libxml2"
CC = gcc
CFLAGS=-Wall $(XML2_LIBDIR) $(XML2_INCLUDEDIR) $(XML2_LIBS)

all:	fvdlParser

fvdlParser:	src/fvdlParser.c
	$(CC) -o fvdlParser src/fvdlParser.c $(CFLAGS)
	mv fvdlParser ./bin

clean:	
	rm -f ./src/*.o
	rm -f ./bin/fvdlParser
