
include ../Makefile.common

LIBS = -lpthread

all: realtime$(DLL)

clean:
	rm -Rf *.o *$(DLL)*

realclean:
	rm -Rf *.o *$(DLL)*

realtime$(DLL): realtime.c
	$(CC) $(shared) $(dllname) -o $@ $(MOD_CFLAGS) $< $(MOD_LDFLAGS)
