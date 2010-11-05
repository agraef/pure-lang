
# Package name and version number:
dist = pure-xml-$(version)
version = 0.4

# Try to guess the installation prefix (this needs GNU make):
prefix = $(patsubst %/bin/pure,%,$(shell which pure 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

# Installation goes into $(libdir)/pure, you can also set this directly
# instead of $(prefix).
libdir = $(prefix)/lib
datadir = $(libdir)/pure

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
XMLLIBS = -lxslt -lxml2
XMLFLAGS = 
# This is needed to link with GMP.
#LDFLAGS = -Wl,--enable-auto-import
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
ifeq "$(findstring -mingw,$(host))" ""
XMLLIBS = $(shell xml2-config --libs) $(shell xslt-config --libs)
XMLFLAGS = $(shell xml2-config --cflags)
endif

# Default CFLAGS are -g -O2, CPPFLAGS, LDFLAGS and LIBS are empty by default.
# These can be set from the command line as usual. Use CFLAGS, CPPFLAGS and
# LDFLAGS for compiler (-O etc.), preprocessor (-I etc.) and linker (-L etc.) 
# options, respectively. LIBS is to be used for additional libraries to be
# linked (-l etc.).

CFLAGS = -g -O2

FLAGS = $(CPPFLAGS) $(CFLAGS) $(PIC) $(LDFLAGS)

DISTFILES = COPYING COPYING.LESSER Makefile README pure-xml.html config.guess \
xml.c xml.pure examples/*.pure examples/*.xml examples/*.xsl

all: xml$(DLL)

xml$(DLL): xml.c
	$(CC) $(shared) -o $@ $< $(XMLFLAGS) $(FLAGS) $(XMLLIBS) -lpure $(LIBS)

clean:
	rm -f *$(DLL) *~ *.a *.o

install:
	test -d "$(DESTDIR)$(datadir)" || mkdir -p "$(DESTDIR)$(datadir)"
	cp xml.pure xml$(DLL) pure-xml.html "$(DESTDIR)$(datadir)"

uninstall:
	rm -f "$(DESTDIR)$(datadir)/xml.pure" "$(DESTDIR)$(datadir)/xml$(DLL)" "$(DESTDIR)$(datadir)/pure-xml.html"

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

# Generate the documentation. This needs docutils.

rst2html = $(shell which rst2html 2>/dev/null || which rst2html.py 2>/dev/null || echo rst2html)
rst2latex = $(shell which rst2latex 2>/dev/null || which rst2latex.py 2>/dev/null || echo rst2latex)

sources = xml.pure
target = pure-xml

pure-doc = pure-doc

$(target).txt: README $(sources)
	$(pure-doc) $(sources) | cat README - > $@

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
