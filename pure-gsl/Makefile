
# Package name and version number:
dist = pure-gsl-$(version)
version = 0.3

# Try to guess the installation prefix (this needs GNU make):
prefix = $(patsubst %/bin/pure,%,$(shell which pure 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

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

DISTFILES = COPYING Makefile README config.guess examples/*.pure gsl.c gsl.pure

all: gsl$(DLL)

gsl$(DLL): gsl.c
	gcc $(shared) -o $@ $< $(PIC) -lgsl -lgslcblas -lm -lpure

clean:
	rm -f *$(DLL) *~ *.a *.o

install:
	test -d "$(DESTDIR)$(prefix)/lib/pure" || mkdir -p "$(DESTDIR)$(prefix)/lib/pure"
	cp gsl.pure gsl$(DLL) "$(DESTDIR)$(prefix)/lib/pure"

uninstall:
	rm -f "$(DESTDIR)$(prefix)/lib/pure/gsl.pure" "$(DESTDIR)$(prefix)/lib/pure/gsl$(DLL)"

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
