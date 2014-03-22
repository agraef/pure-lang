
include ../Makefile.common

LIBS = -lfftw3

.PHONY: all clean realclean

all: fftw$(DLL)

clean:
	rm -Rf *.o *$(DLL)*

realclean:
	rm -Rf *.o *$(DLL)*

fftw$(DLL): fftw.c
	$(CC) $(shared) $(dllname) -o $@ $(MOD_CFLAGS) $< $(MOD_LDFLAGS)
