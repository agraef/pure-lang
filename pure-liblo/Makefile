
# Package name and version number:
dist = pure-liblo-$(version)
version = 0.1

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

DISTFILES = COPYING Makefile README config.guess examples/*.pure lo.c lo.pure \
lo-add.c lo-add.pure pure-liblo.html

all: lo$(DLL)

lo$(DLL): lo.c lo-add.c
	$(CC) $(shared) -o $@ $^ $(FLAGS) -lpure -llo $(LIBS)

clean:
	rm -f *$(DLL) *~ *.a *.o pure-liblo.tex

# documentation (this needs docutils)

html: pure-liblo.html
latex: pure-liblo.tex
pdf: pure-liblo.pdf

pure-liblo.html: README
	rst2html.py $< $@

pure-liblo.tex: README
	rst2latex.py $< $@

pure-liblo.pdf: pure-liblo.tex
	pdflatex $< && pdflatex $< && pdflatex $<
	rm -r *.aux *.log *.out

install:
	test -d "$(DESTDIR)$(libdir)/pure" || mkdir -p "$(DESTDIR)$(libdir)/pure"
	cp lo.pure lo$(DLL) pure-liblo.html "$(DESTDIR)$(libdir)/pure"

uninstall:
	rm -f "$(DESTDIR)$(libdir)/pure/lo.pure" "$(DESTDIR)$(libdir)/pure/lo$(DLL)" "$(DESTDIR)$(libdir)/pure/pure-liblo.html"

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

#############################################################################
# Generator stuff. You only need this if you want to regenerate the wrappers.
# You need pure-gen and the liblo headers to do this.
#############################################################################

# Path to the installed liblo headers (the lo subdir is appended).

# NOTE: The path needs to be valid at generation time only. The generated C
# source uses a relative path which should work on most systems which have
# liblo installed.

includedir = /usr/include

# The following need to be excluded since they are internal and/or varargs
# functions. We provide Purified wrappers in lo-add.c/pure instead.

excludes = '^(lo_message_add|lo_message_add_varargs|lo_send|lo_send_timestamped|lo_send_from|lo_.+_internal|LO_MARKER_A|LO_MARKER_B)$$'

############################################################################

generate:
	rm -rf lo.pure lo.c
	$(MAKE) all

lo.pure lo.c: lo-add.pure
	pure-gen -fc-auto -p lo -m lo -s '$(includedir)/lo/*.h;' -x $(excludes) '$(includedir)/lo/lo.h'
	sed -e 's|#include \"$(includedir)/\(\([A-Za-z-]\+/\)\?[A-Za-z-]\+\.h\)\"|#include <\1>|g' < lo.c > lo.c.new && rm lo.c && mv lo.c.new lo.c
	cat lo.pure lo-add.pure > lo.pure.new && rm lo.pure && mv lo.pure.new lo.pure
