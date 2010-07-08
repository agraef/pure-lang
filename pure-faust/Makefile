
# Package name and version number:
dist = pure-faust-$(version)
version = 0.1

# Try to guess the Pure installation prefix:
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

# Default CXXFLAGS are -g -O2, CPPFLAGS, LDFLAGS and LIBS are empty by default.
# These can be set from the command line as usual. Use CXXFLAGS, CPPFLAGS and
# LDFLAGS for additional compiler (-O etc.), preprocessor (-I etc.) and linker
# (-L etc.) options, respectively. LIBS is to be used for additional libraries
# to be linked (-l etc.).

CXXFLAGS = -g -O2

DISTFILES = COPYING COPYING.LESSER Makefile README pure-faust.html \
config.guess faust.pure faust.cc \
examples/Makefile examples/pure.cpp examples/*.pure examples/*.dsp

all: faust$(DLL)

faust$(DLL): faust.o
	$(CXX) $(shared) -o $@ $(PIC) $(LDFLAGS) $< -lpure -lltdl $(LIBS)

faust.o: faust.cc
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(PIC) -o $@ -c $<

clean:
	rm -f faust$(DLL) *~ *.a *.o *.tex *.dvi *.pdf *.aux *.log *.out

# documentation (this needs docutils)

rst2html = $(shell which rst2html 2>/dev/null || which rst2html.py 2>/dev/null || echo rst2html)
rst2latex = $(shell which rst2latex 2>/dev/null || which rst2latex.py 2>/dev/null || echo rst2latex)

html: pure-faust.html
latex: pure-faust.tex
pdf: pure-faust.pdf

pure-faust.html: README
	$(rst2html) $< $@

pure-faust.tex: README
	$(rst2latex) $< $@

pure-faust.pdf: pure-faust.tex
	pdflatex $<
	pdflatex $<
	rm -f *.aux *.log *.out

install:
	test -d "$(DESTDIR)$(libdir)/pure" || mkdir -p "$(DESTDIR)$(libdir)/pure"
	cp pure-faust.html faust.pure faust$(DLL) "$(DESTDIR)$(libdir)/pure"

uninstall:
	rm -f "$(DESTDIR)$(libdir)/pure"/pure-faust.html "$(DESTDIR)$(libdir)/pure"/faust.pure "$(DESTDIR)$(libdir)/pure"/faust$(DLL)

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
