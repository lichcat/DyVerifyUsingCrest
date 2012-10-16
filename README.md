DyVerifyUsingCrest
add some features to crest-0.1.1 so that we can dynamically verify
some static warnings from Fortify SCA

1. depend on CREST(with CIL1.3.7 included) : http://code.google.com/p/crest/

2. CREST uses the yices SMT solver and CIL , read /pgcrest/README for more information 

3. /fvdlResolve contains the .fvdl format resolver since Fortify SCA generates a report.fvdl file;

   fvdl resolver depends on libxml2 : http://www.xmlsoft.org/, with -L/usr/local/lib 
   and -I/usr/local/include/libxml2 here in /fvdlResolve/Makefile   