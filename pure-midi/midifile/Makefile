
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

all: midifile$(DLL)

clean:
	rm -f *.o *$(DLL)

midifile$(DLL): midifile.c mf.c
	$(CC) $(shared) -o $@ $^ $(FLAGS) -lpure $(LIBS)

# You can use this to regenerate midifile.pure from midifile.h and mf.h using
# pure-gen. CAVEAT: This is for maintainers only and shouldn't normally be
# necessary. Note that the midifile.pure distributed with this package has
# been hand-edited; when regenerating the wrapper you will loose these edits.

midifile.pure: midifile.h mf.h
# only grab the DIVISION_TYPE constants, these are needed to interpret the
# header data
	pure-gen -m midifile -p MIDI_FILE -lmidifile -smidifile.h::MIDI_FILE_DIVISION_TYPE midifile.h -o $@
# the actual Pure interface operations are in mf.c
	pure-gen -N -m midifile -p mf -smf.h:: mf.h -o $@
