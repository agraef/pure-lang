
# Package name and version number:
dist = pure-gsl-$(version)
version = 0.9

# Try to guess the installation prefix (this needs GNU make):
prefix = $(patsubst %/bin/pure,%,$(shell which pure 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

# Installation goes into $(libdir)/pure, you can also set this directly
# instead of $(prefix).
libdir = $(prefix)/lib

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
LDFLAGS = -Wl,--enable-auto-import
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

DISTFILES = COPYING Makefile README config.guess pure-gsl.html examples/*.pure gsl/*.pure gsl.c gsl.pure

all: gsl$(DLL)

gsl$(DLL): gsl.c
	$(CC) $(shared) -o $@ $< $(FLAGS) -lgsl -lgslcblas -lm -lpure $(LIBS)

clean:
	rm -f *$(DLL) *~ *.a *.o

install:
	test -d "$(DESTDIR)$(libdir)/pure" || mkdir -p "$(DESTDIR)$(libdir)/pure"
	mkdir -p "$(DESTDIR)$(libdir)/pure/gsl"
	cp gsl.pure gsl$(DLL) "$(DESTDIR)$(libdir)/pure"
	cp gsl/*.pure "$(DESTDIR)$(libdir)/pure/gsl/"
	cp pure-gsl.html "$(DESTDIR)$(libdir)/pure/"

uninstall:
	rm -f "$(DESTDIR)$(libdir)/pure/gsl.pure" "$(DESTDIR)$(libdir)/pure/gsl$(DLL)"
	rm -R -f "$(DESTDIR)$(libdir)/pure/gsl"

dist:
	rm -rf $(dist)
	mkdir $(dist) && mkdir $(dist)/examples && mkdir $(dist)/gsl
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && make && make install DESTDIR=./BUILD
	rm -rf $(dist)

# Generate the documentation. This needs docutils.
# Keep the files in order of the GSL manual Chapter order.

sources = gsl.pure gsl/poly.pure gsl/sf.pure gsl/matrix.pure gsl/fit.pure gsl/stats.pure gsl/randist.pure gsl/sort.pure
target = pure-gsl

pure-doc = pure-doc

$(target).txt: $(sources)
	$(pure-doc) $(sources) > $@

rst2html = $(shell which rst2html 2>/dev/null || which rst2html.py 2>/dev/null || echo rst2html)
rst2latex = $(shell which rst2latex 2>/dev/null || which rst2latex.py 2>/dev/null || echo rst2latex)

.PHONY: html tex pdf

html: $(target).html
tex: $(target).tex
pdf: $(target).pdf

%.html: %.txt
	$(rst2html) $< $@

%.tex: %.txt
	$(rst2latex) $< $@

# This also requires that you have TeX installed.

%.pdf: %.tex
	pdflatex $<
	pdflatex $<
	rm -f *.aux *.log *.out
