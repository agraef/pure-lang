
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

all: srcprocess$(DLL) samplerate.pure

clean:
	rm -f *.o *$(DLL)

realclean:
	rm -f *.o *$(DLL) samplerate.pure

srcprocess$(DLL): srcprocess.c
	$(CC) $(shared) -o $@ $< $(FLAGS) -lsamplerate -lpure $(LIBS)

includedir = /usr/include

generate:
	rm -f samplerate.pure
	make samplerate.pure

samplerate.pure: srcprocess.h src.pure
	pure-gen -s '$(includedir)/samplerate.h::' $(includedir)/samplerate.h -lsrcprocess -o $@
	pure-gen -N -s srcprocess.h:: srcprocess.h -o $@
	cat src.pure >> $@
