
# Package name and version number:
dist = pd-pure-$(version)
version = 0.3

# Try to guess the installation prefix (this needs GNU make):
prefix = $(patsubst %/bin/pd,%,$(shell which pd 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

# Pd library path.
pdlibdir = $(prefix)/lib/pd

# Install dir for the external and accompanying stuff.
pdextradir = $(pdlibdir)/extra/pure

# Try to guess the host system type.
host = $(shell ./config.guess)

# Platform-specific defaults, edit this as needed.
#PIC = -fPIC # uncomment for x86-64 compilation
DLL = .so
shared = -shared

# Take care of some common systems.
ifneq "$(findstring -mingw,$(host))" ""
# Windows
DLL = .dll
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

DISTFILES = COPYING Makefile README config.guess pure.c examples/*.pure examples/*.pd examples/*.el

all: pure$(DLL)

pure$(DLL): pure.c
	gcc $(shared) -o $@ -DVERSION="\"$(version)\"" -DLIBDIR="\"$(pdlibdir)\"" $< $(PIC) -lpure

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
