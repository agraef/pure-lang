
# Package name and version number:
dist = pd-pure-$(version)
version = 0.5

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
# Windows
DLL = .dll
LIBS = -lpd
LDFLAGS = -Wl,--enable-auto-import
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

DISTFILES = COPYING Makefile README pd-pure.html config.guess pure.c \
examples/*.pure examples/*.pd examples/*.el

all: pure$(DLL)

pure$(DLL): pure.o
	$(CC) $(shared) -o $@ $(PIC) $(LDFLAGS) $< -lpure $(LIBS)

pure.o: pure.c
	$(CC) $(CPPFLAGS) $(CFLAGS) $(PIC) -o $@ -c $< -DVERSION="\"$(version)\"" -DLIBDIR="\"$(pdlibdir)\""

clean:
	rm -f pure$(DLL) *~ *.a *.o *.tex *.dvi *.pdf *.aux *.log *.out

# documentation (this needs docutils)

html: pd-pure.html
latex: pd-pure.tex
pdf: pd-pure.pdf

pd-pure.html: README
	rst2html.py $< $@

pd-pure.tex: README
	rst2latex.py $< $@

pd-pure.pdf: pd-pure.tex
	pdflatex $<
	rm -f *.aux *.log *.out

install:
	test -d "$(DESTDIR)$(pdextradir)" || mkdir -p "$(DESTDIR)$(pdextradir)"
	cp COPYING README pd-pure.html Makefile pure.c pure$(DLL) examples/*.pure examples/*.pd examples/*.el "$(DESTDIR)$(pdextradir)"
	test -d "$(DESTDIR)$(puredatadir)" || mkdir -p "$(DESTDIR)$(puredatadir)"
	ln -sf "$(pdextradir)"/pd-pure.html "$(DESTDIR)$(puredatadir)"

uninstall:
	rm -rf "$(DESTDIR)$(pdextradir)" "$(DESTDIR)$(puredatadir)"/pd-pure.html

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
