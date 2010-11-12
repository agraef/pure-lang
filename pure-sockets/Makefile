
# Package name and version number:
pkgname = pure-sockets
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

# Anything listed below will also be included in the tarball. Adjust as needed.
extradirs = examples
extradist = examples/*.pure

# Distribution files for dist and distcheck targets below.
distfiles = COPYING COPYING.LESSER Makefile README $(source) \
$(extradist)

# Installation files for install and uninstall targets.
instfiles = $(pure_source) $(bitcode)

# The stuff below is pretty generic. It will build bitcode modules for all C
# and C++ sources in this directory, and the documentation in html format from
# the README.

#CC = llvm-gcc
CC = clang
#CXX = llvm-g++
CXX = clang++

# NOTE: The combination -g -O2 doesn't seem to work with clang 2.8.
# llvm-gcc -g -O2 seems to be fine, though.

#CFLAGS = -g -O2
#CXXFLAGS = -g -O2
CFLAGS = -O2
CXXFLAGS = -O2

BCFLAGS = -emit-llvm

pure_source := $(wildcard *.pure)
c_source    := $(wildcard *.c)
cc_source   := $(wildcard *.cc)
bitcode     := $(c_source:.c=.bc) $(cc_source:.cc=.bc)
source      := $(c_source) $(cc_source) $(pure_source)

all: $(bitcode)

clean:
	rm -f $(bitcode)

# compile C source to LLVM bitcode
%.bc: %.c
	-$(CC) $(CFLAGS) $(BCFLAGS) -c $< -o $@

# compile C++ source to LLVM bitcode
%.bc: %.cc
	-$(CXX) $(CXXFLAGS) $(BCFLAGS) -c $< -o $@

# install and uninstall targets.

pkgdir = $(DESTDIR)$(libdir)/pure

install: $(instfiles)
	test -d "$(pkgdir)" || mkdir -p "$(pkgdir)"
	cp $(instfiles) "$(pkgdir)"

uninstall:
	for x in $(instfiles); do rm -f "$(pkgdir)/$$x"; done

# dist and distcheck targets.

dist = $(pkgname)-$(version)

dist: $(html)
	rm -rf $(dist)
	mkdir $(dist)
	for x in $(extradirs); do mkdir $(dist)/$$x; done
	for x in $(distfiles); do ln -sf $$PWD/$$x $(dist)/$$x; done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && make && make install DESTDIR=./BUILD
	rm -rf $(dist)
