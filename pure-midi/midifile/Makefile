
# Try to guess the host system type.
host = $(shell ../config.guess)

prefix = $(patsubst %/bin/pure$(EXE),%,$(shell which pure 2>/dev/null))
ifeq ($(strip $(prefix)),)
prefix = /usr/local
endif
libdir = $(prefix)/lib

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
# OSX
DLL = .dylib
shared = -dynamiclib
dllname = -install_name "$(libdir)/pure/midifile$(DLL)"
# MacPorts defaults
CPPFLAGS = -I/opt/local/include
LDFLAGS = -L/opt/local/lib
PUREGEN_FLAGS = -I/opt/local/include
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

all: midifile$(DLL)

clean:
	rm -f *.o *$(DLL)

midifile$(DLL): midifile.c mf.c
	$(CC) $(shared) $(dllname) -o $@ $^ $(FLAGS) -lpure $(LIBS)

# You can use this to regenerate midifile.pure from mf.h using pure-gen.
# CAVEAT: This is for maintainers only and shouldn't normally be necessary.
# Note that the midifile.pure distributed with this package has been
# hand-edited; when regenerating the wrapper you will loose these edits.

midifile.pure: midifile.h mf.h
	pure-gen -m midifile -p mf -smf.h:: mf.h -o $@
