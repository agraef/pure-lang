
# Try to guess the host system type.
host = $(shell ../config.guess)

# compiler flags -- these can be overridden from the command line if needed,
# as well as CPPFLAGS, LDFLAGS and LIBS
CFLAGS = -O3
#CFLAGS = -g -O2

# platform-specific setup

DLL         = $(shell pkg-config pure --variable DLL)
PIC         = $(shell pkg-config pure --variable PIC)
shared      = $(shell pkg-config pure --variable shared)

prefix      = $(shell pkg-config pure --variable prefix)
bindir      = $(prefix)/bin
libdir      = $(shell pkg-config pure --variable libdir)

ifeq ($(DLL),.dylib)
dllname = -install_name "$(libdir)/pure/$@"
endif

MOD_CFLAGS  = $(PIC) $(shell pkg-config pure --cflags) $(CFLAGS) $(CPPFLAGS)
MOD_LDFLAGS = $(shell pkg-config pure --libs) $(LDFLAGS) $(LIBS)

all: midifile$(DLL)

clean:
	rm -Rf *.o *$(DLL)*

midifile$(DLL): midifile.c mf.c
	$(CC) $(shared) $(dllname) -o $@ $(MOD_CFLAGS) $^ $(MOD_LDFLAGS)

# You can use this to regenerate midifile.pure from mf.h using pure-gen.
# CAVEAT: This is for maintainers only and shouldn't normally be necessary.
# Note that the midifile.pure distributed with this package has been
# hand-edited; when regenerating the wrapper you will loose these edits.

midifile.pure: midifile.h mf.h
	pure-gen -m midifile -p mf -smf.h:: mf.h -o $@
