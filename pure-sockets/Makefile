
# Package name and version number:
pkgname = pure-sockets
version = 0.6

# Try to guess the host system type.
host = $(shell ./config.guess)

# Try to guess the installation prefix (this needs GNU make):
prefix = $(patsubst %/bin/pure$(EXE),%,$(shell which pure 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

# Platform-specific defaults, edit this as needed.
#PIC = -fPIC # uncomment for x86-64 compilation
DLL = .so
shared = -shared

# Take care of some common systems.
ifneq "$(findstring -mingw,$(host))" ""
# Windows
DLL = .dll
EXE = .exe
LIBS = -lws2_32
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

# Installation goes into $(libdir)/pure, you can also set this directly
# instead of $(prefix).
libdir = $(prefix)/lib

# Anything listed below will also be included in the tarball. Adjust as needed.
extradirs = examples debian
extradist = examples/*.pure debian/*

# Distribution files for dist and distcheck targets below.
distfiles = COPYING COPYING.LESSER Makefile README config.guess $(source) $(extradist)
sedfiles = README

# Installation files for install and uninstall targets.
instfiles = $(pure_source) $(objects)

# The stuff below is pretty generic.

#CC = gcc
#CXX = g++

CFLAGS = -g -O2
CXXFLAGS = -g -O2

pure_source := $(wildcard *.pure)
c_source    := $(wildcard *.c)
cc_source   := $(wildcard *.cc)
objects     := $(c_source:.c=$(DLL)) $(cc_source:.cc=$(DLL))
source      := $(c_source) $(cc_source) $(pure_source)

all: $(objects)

clean:
	rm -f $(objects)

# compile C source to shared library
%$(DLL): %.c
	$(CC) $(shared) $(CPPFLAGS) $(CFLAGS) $(PIC) $< -o $@ $(LDFLAGS) -lpure $(LIBS)

# compile C++ source to shared library
%$(DLL): %.cc
	$(CXX) $(shared) $(CPPFLAGS) $(CXXFLAGS) $(PIC) $< -o $@ $(LDFLAGS) -lpure $(LIBS)

# install and uninstall targets.

pkgdir = $(DESTDIR)$(libdir)/pure

install: $(instfiles)
	test -d "$(pkgdir)" || mkdir -p "$(pkgdir)"
	cp $(instfiles) "$(pkgdir)"

uninstall:
	for x in $(instfiles); do rm -f "$(pkgdir)/$$x"; done

# dist and distcheck targets.

dist = $(pkgname)-$(version)

date = $(shell date "+%B %-d, %Y")
datesubst = sed -e "s?@version@?$(version)?g" -e "s?|today|?$(date)?g" < $(1) > $(2)

dist: $(html)
	rm -rf $(dist)
	mkdir $(dist)
	for x in $(extradirs); do mkdir $(dist)/$$x; done
	for x in $(distfiles); do ln -sf $$PWD/$$x $(dist)/$$x; done
	for x in $(sedfiles); do rm -f $(dist)/$$x; $(call datesubst,$$PWD/$$x,$(dist)/$$x); done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && make && make install DESTDIR=./BUILD
	rm -rf $(dist)

debsrc = $(shell echo $(dist) | sed -e 's/-$(version)/_$(version)/').orig.tar.gz

deb: $(debsrc) dist
	tar xfz $(dist).tar.gz
	cd $(dist) && debuild $(DEBUILD_FLAGS)
	rm -rf $(dist)

$(debsrc):
	wget -nv https://bitbucket.org/purelang/pure-lang/downloads/$(dist).tar.gz -O $@
