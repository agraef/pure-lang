
# Package name and version number:
dist = pure-tk-$(version)
version = 0.1

# Try to guess the installation prefix (this needs GNU make):
prefix = $(patsubst %/bin/pure,%,$(shell which pure 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

# Try to guess the Tcl version.
tclvers = $(shell echo 'puts $$tcl_version' | tclsh)
ifeq ($(strip $(tclvers)),)
tclvers = 8.5
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

DISTFILES = COPYING Makefile README pure-tk.html config.guess tk.c tk.pure \
gnocl.pure examples/Makefile examples/*.pure examples/*.tcl examples/*.glade

all: tk$(DLL)

tk$(DLL): tk.c
	$(CC) $(shared) -o $@ -Wall $< $(FLAGS) -ltcl$(tclvers) -ltk$(tclvers) -lpure $(LIBS)

clean:
	rm -f *$(DLL) *~ *.a *.o *.tex *.pdf *.aux *.log *.out

# documentation (this needs docutils)

rst2html = $(shell which rst2html 2>/dev/null || which rst2html.py 2>/dev/null || echo rst2html)
rst2latex = $(shell which rst2latex 2>/dev/null || which rst2latex.py 2>/dev/null || echo rst2latex)

html: pure-tk.html
latex: pure-tk.tex
pdf: pure-tk.pdf

pure-tk.html: README
	$(rst2html) $< $@

pure-tk.tex: README
	$(rst2latex) $< $@

pure-tk.pdf: pure-tk.tex
	pdflatex $<
	pdflatex $<
	rm -f *.aux *.log *.out

install:
	test -d "$(DESTDIR)$(libdir)/pure" || mkdir -p "$(DESTDIR)$(libdir)/pure"
	cp gnocl.pure tk.pure tk$(DLL) pure-tk.html "$(DESTDIR)$(libdir)/pure"

uninstall:
	rm -f "$(DESTDIR)$(libdir)/pure/gnocl.pure" "$(DESTDIR)$(libdir)/pure/tk.pure" "$(DESTDIR)$(libdir)/pure/tk$(DLL)" "$(DESTDIR)$(libdir)/pure/pure-tk.html"

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
