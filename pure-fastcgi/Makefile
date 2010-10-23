
# Package name and version number:
dist = pure-fastcgi-$(version)
version = 0.2

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
# Some systems have the headers in /usr/include/fastcgi. Adjust as needed.
INCLUDES = -I/usr/include/fastcgi

# Take care of some common systems.
ifneq "$(findstring -mingw,$(host))" ""
# Windows
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

DISTFILES = COPYING Makefile README pure-fastcgi.html config.guess \
examples/*.pure fastcgi.c fastcgi.pure

all: fastcgi$(DLL)

fastcgi$(DLL): fastcgi.c
	$(CC) $(shared) -o $@ $(INCLUDES) $< $(FLAGS) -lfcgi -lpure $(LIBS)

clean:
	rm -f *$(DLL) *~ *.a *.o *.tex *.pdf *.aux *.log *.out

# documentation (this needs docutils)

rst2html = $(shell which rst2html 2>/dev/null || which rst2html.py 2>/dev/null || echo rst2html)
rst2latex = $(shell which rst2latex 2>/dev/null || which rst2latex.py 2>/dev/null || echo rst2latex)

html: pure-fastcgi.html
latex: pure-fastcgi.tex
pdf: pure-fastcgi.pdf

pure-fastcgi.html: README
	$(rst2html) $< $@

pure-fastcgi.tex: README
	$(rst2latex) $< $@

pure-fastcgi.pdf: pure-fastcgi.tex
	pdflatex $<
	pdflatex $<
	rm -f *.aux *.log *.out

install:
	test -d "$(DESTDIR)$(libdir)/pure" || mkdir -p "$(DESTDIR)$(libdir)/pure"
	cp fastcgi.pure fastcgi$(DLL) pure-fastcgi.html "$(DESTDIR)$(libdir)/pure"

uninstall:
	rm -f "$(DESTDIR)$(libdir)/pure/fastcgi.pure" "$(DESTDIR)$(libdir)/pure/fastcgi$(DLL)" "$(DESTDIR)$(libdir)/pure/pure-fastcgi.html"

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
