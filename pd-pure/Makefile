
# Package name and version number:
dist = pd-pure-$(version)
version = 0.10

# Here are some variables you can specify when invoking 'make', besides the
# usual CXXFLAGS, CPPFLAGS etc.:

# VERBOSE=-DVERBOSE: This enables special code to make pd-pure give some
# feedback on stdout when it's compiling scripts. This is disabled by default.

# EAGER=-DEAGER: This enables special code to enforce eager compilation at
# script load time. This is enabled automatically for Pure versions which
# support this (0.34 and later), thus you shouldn't have to set this flag
# manually. However, you can forcibly disable this option (not recommended) by
# passing 'EAGER='.

# Try to guess the Pd installation prefix (this needs GNU make):
prefix = $(patsubst %/bin/pd,%,$(shell which pd 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

# Try to guess the Pure installation prefix (this is needed to install the
# documentation):
pureprefix = $(patsubst %/bin/pure,%,$(shell which pure 2>/dev/null))
ifeq ($(strip $(pureprefix)),)
# Fall back to /usr/local.
pureprefix = /usr/local
endif

# Try to guess the Pure minor version number, and set up the EAGER option
# accordingly:
pureminor = $(shell pure --version 2>/dev/null | sed -n -e 's/^Pure 0\.\([0-9]*\).*/\1/' -e '/^[0-9]*$$/p' 2>/dev/null)
EAGER = $(shell (test -z "$(pureminor)" || test "$(pureminor)" -ge 34) && echo '-DEAGER')

# Installation goes into $(libdir)/pd, you can also set this directly
# instead of $(prefix).
libdir = $(prefix)/lib

# Pd library path.
pdlibdir = $(libdir)/pd

# Install dir for the external and accompanying stuff.
pdextradir = $(pdlibdir)/extra/pure

# Install dir for the documentation.
purelibdir = $(pureprefix)/lib
puredatadir = $(purelibdir)/pure

# Try to guess the host system type.
host = $(shell ./config.guess)

# Platform-specific defaults, edit this as needed.
#PIC = -fPIC # uncomment for x86-64 compilation
DLL = .so
shared = -shared

# Take care of some common systems.
ifneq "$(findstring -mingw,$(host))" ""
# Windows (this assumes that a working Pd installation lives in $(PDDIR))
PDDIR=/c/progra~1/pd
DLL = .dll
LIBS = -lpd
LDFLAGS = -Wl,--enable-auto-import -L$(PDDIR)/lib
CPPFLAGS = -I$(PDDIR)/include
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
# If Pure 0.34 or later is used, -DEAGER is added to CPPFLAGS. These variables
# can be set from the command line as usual. Use CFLAGS, CPPFLAGS and LDFLAGS
# for additional compiler (-O etc.), preprocessor (-I etc.) and linker (-L
# etc.) options, respectively. LIBS is to be used for additional libraries to
# be linked (-l etc.).

CFLAGS = -g -O2
ifneq "$(EAGER)" ""
override CPPFLAGS += $(EAGER)
endif
ifneq "$(VERBOSE)" ""
override CPPFLAGS += $(VERBOSE)
endif

DISTFILES = COPYING Makefile README config.guess pure.c \
examples/*.pure examples/*.pd examples/*.el

all: pure$(DLL)

pure$(DLL): pure.o
	$(CC) $(shared) -o $@ $(PIC) $(LDFLAGS) $< -lpure $(LIBS)

pure.o: pure.c
	$(CC) $(CPPFLAGS) $(CFLAGS) $(PIC) -o $@ -c $< -DVERSION="\"$(version)\"" -DLIBDIR="\"$(pdlibdir)\""

clean:
	rm -f pure$(DLL) *~ *.a *.o *.tex *.dvi *.pdf *.aux *.log *.out

# documentation (this needs docutils)

rst2html = $(shell which rst2html 2>/dev/null || which rst2html.py 2>/dev/null || echo rst2html)
rst2latex = $(shell which rst2latex 2>/dev/null || which rst2latex.py 2>/dev/null || echo rst2latex)

html: pd-pure.html
latex: pd-pure.tex
pdf: pd-pure.pdf

pd-pure.html: README
	$(rst2html) $< $@

pd-pure.tex: README
	$(rst2latex) $< $@

pd-pure.pdf: pd-pure.tex
	pdflatex $<
	pdflatex $<
	rm -f *.aux *.log *.out

install:
	test -d "$(DESTDIR)$(pdextradir)" || mkdir -p "$(DESTDIR)$(pdextradir)"
	cp COPYING README Makefile pure.c pure$(DLL) examples/*.pure examples/*.pd examples/*.el "$(DESTDIR)$(pdextradir)"
	test -d "$(DESTDIR)$(puredatadir)" || mkdir -p "$(DESTDIR)$(puredatadir)"

uninstall:
	rm -rf "$(DESTDIR)$(pdextradir)"

dist:
	rm -rf $(dist)
	mkdir $(dist) && mkdir $(dist)/examples
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && make && make install DESTDIR=./BUILD
	rm -rf $(dist)
