
# Try to guess the host system type.
host = $(shell ../config.guess)

# Platform-specific defaults, edit this as needed.
#PIC = -fPIC # uncomment for x86-64 compilation
DLL = .so
shared = -shared

# Take care of some common systems.
ifneq "$(findstring -mingw,$(host))" ""
# Windows (untested)
DLL = .dll
endif
ifneq "$(findstring -darwin,$(host))" ""
# OSX (untested)
DLL = .dylib
shared = -dynamiclib
endif
ifneq "$(findstring x86_64-,$(host))" ""
# 64 bit, needs -fPIC flag
PIC = -fPIC
endif

# Default CFLAGS are -g -O2, CPPFLAGS, LDFLAGS and LIBS are empty by default.
# These can be set from the command line as usual. Use CFLAGS, CPPFLAGS and
# LDFLAGS for compiler (-O etc.), preprocessor (-I etc.) and linker (-L etc.) 
# options, respectively. LIBS is to be used for additional libraries to be
# linked (-l etc.).

CFLAGS = -g -O2

FLAGS = $(CPPFLAGS) $(CFLAGS) $(PIC) $(LDFLAGS)

.PHONY: all clean realclean generate

all: sfinfo$(DLL) sndfile.pure

clean:
	rm -f *.o *$(DLL)

realclean:
	rm -f *.o *$(DLL) sndfile.pure

sfinfo$(DLL): sfinfo.c
	$(CC) $(shared) -o $@ $< $(FLAGS) -lsndfile -lpure $(LIBS)

includedir = /usr/include

generate:
	rm -f sndfile.pure
	make sndfile.pure

# NOTE: sf_open, sf_open_fd and sf_close are excluded here since we provide
# wrappers for those. (sf_open_virtual is not supported in the Pure wrapper.)

sndfile.pure: $(includedir)/sndfile.h sfinfo.h sf.pure
	pure-gen -s '$(includedir)/sndfile.h::' -x '(sf_open|sf_close)' $< -lsfinfo -o $@
	pure-gen -N -s sfinfo.h:: sfinfo.h -o $@
	cat sf.pure >> $@
