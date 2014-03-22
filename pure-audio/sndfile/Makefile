
include ../Makefile.common

LIBS = -lsndfile

.PHONY: all clean realclean generate

all: sfinfo$(DLL) sndfile.pure

clean:
	rm -Rf *.o *$(DLL)*

realclean:
	rm -Rf *.o *$(DLL)* sndfile.pure

sfinfo$(DLL): sfinfo.c
	$(CC) $(shared) $(dllname) -o $@ $(MOD_CFLAGS) $< $(MOD_LDFLAGS)

includedir = /usr/include

generate:
	rm -f sndfile.pure
	make sndfile.pure

# NOTE: sf_open, sf_open_fd and sf_close are excluded here since we provide
# wrappers for those. (sf_open_virtual is not supported in the Pure wrapper.)

sndfile.pure: sfinfo.h sf.pure
	pure-gen -C -ansi -s '$(includedir)/sndfile.h;' -x '(sf_open|sf_close)'  $(includedir)/sndfile.h -lsfinfo -o $@
	pure-gen -C -ansi -N -s 'sfinfo.h;' sfinfo.h -o $@
	cat sf.pure >> $@
ifeq ($(DLL),.dll)
	mv $@ $@.tmp
	tr -d '\r' < $@.tmp > $@
	rm -f $@.tmp
endif
