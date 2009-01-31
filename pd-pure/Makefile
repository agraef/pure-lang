
# Package name and version number:
dist = pd-pure-$(version)
version = 0.3

# Try to guess the installation prefix (this needs GNU make):
prefix = $(patsubst %/bin/pd,%,$(shell which pd 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

# Installation goes into $(libdir)/pd, you can also set this directly
# instead of $(prefix).
libdir = $(prefix)/lib

# Pd library path.
pdlibdir = $(libdir)/pd

# Install dir for the external and accompanying stuff.
pdextradir = $(pdlibdir)/extra/pure

# Try to guess the host system type.
host = $(shell ./config.guess)

# Platform-specific defaults, edit this as needed.
#PIC = -fPIC # uncomment for x86-64 compilation
DLL = .so
shared = -shared

# Take care of some common (and broken-by-design) systems.
ifneq "$(findstring -mingw,$(host))" ""
# Windows: "Abandon all hope, ye who enter here." Apparently Windoze isn't
# clever enough to resolve libpure.dll and all its dependencies correctly when
# it's linked into another dll to be loaded with Window's horrible excuse of a
# dynamic loader. So we have to link the interpreter *statically*. Oh well. :(
# NOTE: You have to make sure that you have the compiled Pure source tree
# sitting in $(PUREDIR) when compiling this module.
DLL = .dll
LDFLAGS = -Wl,--enable-auto-import
LIBS = $(PUREOBJ) $(PURELIB) -lpd
PUREDIR = ../pure
PUREOBJ = $(addprefix $(PUREDIR)/, expr.o interpreter.o lexer.o matcher.o parser.o printer.o runtime.o symtable.o util.o)
PURELIB = `llvm-config --ldflags --libs core jit native` -lgsl -lgslcblas -lregex -lglob -liconv -lm -lreadline -lgmp -limagehlp -lpsapi -lmingwex -lstdc++
endif
ifneq "$(findstring -linux,$(host))" ""
# Linux
DLL = .pd_linux
endif
ifneq "$(findstring -darwin,$(host))" ""
# OSX (untested)
DLL = .pd_darwin
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

DISTFILES = COPYING Makefile README config.guess pure.c examples/*.pure examples/*.pd examples/*.el

all: pure$(DLL)

pure$(DLL): pure.o
	gcc $(shared) -o $@ $(PIC) $(LDFLAGS) $< $(LIBS)

pure.o: pure.c
	gcc $(CPPFLAGS) $(CFLAGS) $(PIC) -o $@ -c $< -DVERSION="\"$(version)\"" -DLIBDIR="\"$(pdlibdir)\""

clean:
	rm -f pure$(DLL) *~ *.a *.o

install:
	test -d "$(DESTDIR)$(pdextradir)" || mkdir -p "$(DESTDIR)$(pdextradir)"
	cp COPYING README Makefile pure.c pure$(DLL) examples/*.pure examples/*.pd examples/*.el "$(DESTDIR)$(pdextradir)"

uninstall:
	rm -rf "$(DESTDIR)$(pdextradir)"

dist:
	rm -rf $(dist)
	mkdir $(dist) && mkdir $(dist)/examples
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)
